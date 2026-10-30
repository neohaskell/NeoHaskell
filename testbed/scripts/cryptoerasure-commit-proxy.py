import argparse
import json
import socket
import threading
from pathlib import Path


def receive_exact(connection, size):
    chunks = []
    remaining = size
    while remaining:
        chunk = connection.recv(remaining)
        if not chunk:
            raise EOFError
        chunks.append(chunk)
        remaining -= len(chunk)
    return b"".join(chunks)


def receive_message(connection):
    kind = receive_exact(connection, 1)
    length = receive_exact(connection, 4)
    payload = receive_exact(connection, int.from_bytes(length, "big") - 4)
    return kind, kind + length + payload, payload


class CommitProxy:
    def __init__(self, upstream_port, evidence):
        self.upstream_port = upstream_port
        self.evidence = evidence
        self.lock = threading.Lock()
        self.dropped = False

    def handle(self, client):
        server = socket.create_connection(("127.0.0.1", self.upstream_port))
        state = {"statements": {}, "portals": {}, "pending": False}
        try:
            while True:
                length = receive_exact(client, 4)
                packet = length + receive_exact(client, int.from_bytes(length, "big") - 4)
                server.sendall(packet)
                if packet[4:] in (b"\x04\xd2\x16\x2f", b"\x04\xd2\x16\x30"):
                    client.sendall(receive_exact(server, 1))
                else:
                    break

            def upstream():
                try:
                    while True:
                        kind, packet, payload = receive_message(client)
                        if kind == b"P":
                            name, query, _ = payload.split(b"\x00", 2)
                            state["statements"][name] = b"WITH inserted_keys AS" in query
                        elif kind == b"B":
                            portal, statement, _ = payload.split(b"\x00", 2)
                            state["portals"][portal] = state["statements"].get(statement, False)
                        elif kind == b"E":
                            portal = payload.split(b"\x00", 1)[0]
                            with self.lock:
                                state["pending"] = state["portals"].get(portal, False) and not self.dropped
                        server.sendall(packet)
                except (EOFError, OSError):
                    pass
                finally:
                    server.close()

            threading.Thread(target=upstream, daemon=True).start()
            pending = []
            while True:
                kind, packet, _ = receive_message(server)
                if state["pending"]:
                    pending.append((kind, packet))
                    if kind == b"Z":
                        completed = any(item_kind == b"C" for item_kind, _ in pending)
                        failed = any(item_kind == b"E" for item_kind, _ in pending)
                        with self.lock:
                            should_drop = completed and not failed and not self.dropped
                            if should_drop:
                                self.dropped = True
                        if should_drop:
                            self.evidence.write_text(json.dumps({"dropped_after_commit": True}) + "\n")
                            return
                        for _, buffered in pending:
                            client.sendall(buffered)
                        pending.clear()
                        state["pending"] = False
                else:
                    client.sendall(packet)
        except (EOFError, OSError):
            pass
        finally:
            client.close()
            server.close()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--listen", type=int, required=True)
    parser.add_argument("--upstream", type=int, required=True)
    parser.add_argument("--evidence", type=Path, required=True)
    arguments = parser.parse_args()
    with socket.socket() as listener:
        listener.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        listener.bind(("127.0.0.1", arguments.listen))
        listener.listen()
        print("READY", flush=True)
        proxy = CommitProxy(arguments.upstream, arguments.evidence)
        while True:
            client, _ = listener.accept()
            threading.Thread(target=proxy.handle, args=(client,), daemon=True).start()


if __name__ == "__main__":
    main()
