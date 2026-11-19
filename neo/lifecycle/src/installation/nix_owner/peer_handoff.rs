const DOMAIN: &[u8; 16] = b"NEO-HANDOFF-V001";
const FRAME_BYTES: usize = 48;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct PeerIdentity {
    pid: i32,
    uid: u32,
    gid: u32,
}

struct PeerExpectation {
    identity: PeerIdentity,
    epoch: [u8; 32],
}

#[derive(Debug, PartialEq, Eq)]
pub(in crate::installation) enum HandoffError {
    Transport,
    Timeout,
    CleanupUnknown,
    Cancelled,
    ForegroundLost,
    AdministratorUnavailable,
    Socket,
    Ancillary,
    Truncated,
    Peer,
    Frame,
    Executable,
    Exit,
}

fn verify(
    expected: &PeerExpectation,
    connected: PeerIdentity,
    packet: &[PeerIdentity],
    frame: &[u8],
    truncated: bool,
) -> Result<(), HandoffError> {
    verify_domain(expected, connected, packet, frame, truncated, DOMAIN)
}

fn verify_domain(
    expected: &PeerExpectation,
    connected: PeerIdentity,
    packet: &[PeerIdentity],
    frame: &[u8],
    truncated: bool,
    domain: &[u8; 16],
) -> Result<(), HandoffError> {
    if truncated {
        return Err(HandoffError::Truncated);
    }
    if packet.len() != 1 {
        return Err(HandoffError::Ancillary);
    }
    if expected.identity.pid <= 0
        || connected != expected.identity
        || packet[0] != expected.identity
    {
        return Err(HandoffError::Peer);
    }
    verify_frame(expected, frame, domain)
}

fn verify_frame(
    expected: &PeerExpectation,
    frame: &[u8],
    domain: &[u8; 16],
) -> Result<(), HandoffError> {
    if frame.len() != FRAME_BYTES || &frame[..16] != domain || frame[16..] != expected.epoch {
        return Err(HandoffError::Frame);
    }
    Ok(())
}

#[cfg(target_os = "linux")]
mod linux {
    use super::*;
    use std::mem::{MaybeUninit, size_of, size_of_val, zeroed};
    use std::os::fd::{AsRawFd, OwnedFd};
    use std::ptr;

    pub(super) fn connected_identity(socket: &OwnedFd) -> Result<PeerIdentity, HandoffError> {
        Ok(identity(option::<libc::ucred>(
            socket.as_raw_fd(),
            libc::SO_PEERCRED,
        )?))
    }

    fn identity(value: libc::ucred) -> PeerIdentity {
        PeerIdentity {
            pid: value.pid,
            uid: value.uid,
            gid: value.gid,
        }
    }

    fn option<T: Copy>(fd: i32, name: i32) -> Result<T, HandoffError> {
        let mut value = MaybeUninit::<T>::uninit();
        let mut width = size_of::<T>() as libc::socklen_t;
        let result = unsafe {
            libc::getsockopt(
                fd,
                libc::SOL_SOCKET,
                name,
                value.as_mut_ptr().cast(),
                &mut width,
            )
        };
        if result != 0 || width as usize != size_of::<T>() {
            return Err(HandoffError::Socket);
        }
        Ok(unsafe { value.assume_init() })
    }

    fn aligned(width: usize) -> usize {
        let alignment = size_of::<usize>();
        (width + alignment - 1) & !(alignment - 1)
    }

    fn credentials(control: &[usize], length: usize) -> Result<Vec<PeerIdentity>, HandoffError> {
        if length > size_of_val(control) {
            return Err(HandoffError::Ancillary);
        }
        let mut peers = Vec::with_capacity(1);
        let mut invalid = false;
        let mut offset = 0;
        let header_width = aligned(size_of::<libc::cmsghdr>());
        let bytes = unsafe { std::slice::from_raw_parts(control.as_ptr().cast::<u8>(), length) };
        while offset + size_of::<libc::cmsghdr>() <= length {
            let header =
                unsafe { ptr::read_unaligned(bytes.as_ptr().add(offset).cast::<libc::cmsghdr>()) };
            if header.cmsg_len < header_width || header.cmsg_len > length - offset {
                return Err(HandoffError::Ancillary);
            }
            let data = &bytes[offset + header_width..offset + header.cmsg_len];
            if header.cmsg_level == libc::SOL_SOCKET && header.cmsg_type == libc::SCM_RIGHTS {
                for descriptor in data.chunks_exact(size_of::<i32>()) {
                    let fd = i32::from_ne_bytes(descriptor.try_into().unwrap());
                    unsafe {
                        libc::close(fd);
                    }
                }
                invalid = true;
            } else if header.cmsg_level == libc::SOL_SOCKET
                && header.cmsg_type == libc::SCM_CREDENTIALS
                && data.len() == size_of::<libc::ucred>()
            {
                peers.push(identity(unsafe {
                    ptr::read_unaligned(data.as_ptr().cast::<libc::ucred>())
                }));
            } else {
                invalid = true;
            }
            let next = offset + aligned(header.cmsg_len);
            if next >= length {
                offset = length;
                break;
            }
            offset = next;
        }
        if invalid || offset != length || peers.len() != 1 {
            return Err(HandoffError::Ancillary);
        }
        Ok(peers)
    }

    pub(super) fn receive_inner(
        socket: &OwnedFd,
        expected: &PeerExpectation,
    ) -> Result<(), HandoffError> {
        receive_domain(socket, expected, DOMAIN)
    }

    pub(super) fn receive_domain(
        socket: &OwnedFd,
        expected: &PeerExpectation,
        domain: &[u8; 16],
    ) -> Result<(), HandoffError> {
        receive_checked(socket, expected, domain, None).map(|_| ())
    }

    pub(super) fn receive_policy(
        socket: &OwnedFd,
        expected: &PeerExpectation,
    ) -> Result<u8, HandoffError> {
        receive_checked(
            socket,
            expected,
            b"NEO-POLICY-V001\0",
            Some(|value| super::super::policy_observation::PolicyResult::decode(value).is_some()),
        )
    }

    pub(super) fn receive_policy_inspection(
        socket: &OwnedFd,
        expected: &PeerExpectation,
    ) -> Result<u8, HandoffError> {
        receive_checked(
            socket,
            expected,
            super::launcher::INSPECTION,
            Some(|value| {
                super::super::policy_observation::PolicyInspectionResult::decode(value).is_some()
            }),
        )
    }

    pub(super) fn receive_discovery(
        socket: &OwnedFd,
        expected: &PeerExpectation,
    ) -> Result<[u8; 65], HandoffError> {
        let fd = socket.as_raw_fd();
        if option::<i32>(fd, libc::SO_DOMAIN)? != libc::AF_UNIX
            || option::<i32>(fd, libc::SO_TYPE)? != libc::SOCK_SEQPACKET
            || option::<i32>(fd, libc::SO_PASSCRED)? != 1
        {
            return Err(HandoffError::Socket);
        }
        let connected = identity(option::<libc::ucred>(fd, libc::SO_PEERCRED)?);
        if connected != expected.identity {
            return Err(HandoffError::Peer);
        }
        let mut poll = libc::pollfd {
            fd,
            events: libc::POLLIN,
            revents: 0,
        };
        match unsafe { libc::poll(&mut poll, 1, 1000) } {
            0 => return Err(HandoffError::Timeout),
            1 if poll.revents & libc::POLLIN != 0 => (),
            _ => return Err(HandoffError::Transport),
        }
        let mut frame = [0u8; FRAME_BYTES + 65];
        let mut control = [0usize; 64];
        let packet = read_packet(socket, &mut frame, &mut control)?;
        let peers = credentials(&control, packet.control_bytes)?;
        if packet.count != frame.len() {
            return Err(HandoffError::Frame);
        }
        verify_domain(
            expected,
            connected,
            &peers,
            &frame[..FRAME_BYTES],
            packet.truncated,
            super::launcher::DISCOVER,
        )?;
        Ok(frame[FRAME_BYTES..].try_into().unwrap())
    }

    fn receive_checked(
        socket: &OwnedFd,
        expected: &PeerExpectation,
        domain: &[u8; 16],
        validate_result: Option<fn(u8) -> bool>,
    ) -> Result<u8, HandoffError> {
        let fd = socket.as_raw_fd();
        if option::<i32>(fd, libc::SO_DOMAIN)? != libc::AF_UNIX
            || option::<i32>(fd, libc::SO_TYPE)? != libc::SOCK_SEQPACKET
        {
            return Err(HandoffError::Socket);
        }
        let connected = identity(option::<libc::ucred>(fd, libc::SO_PEERCRED)?);
        if connected != expected.identity {
            return Err(HandoffError::Peer);
        }
        if option::<i32>(fd, libc::SO_PASSCRED)? != 1 {
            return Err(HandoffError::Socket);
        }
        let mut poll = libc::pollfd {
            fd,
            events: libc::POLLIN,
            revents: 0,
        };
        match unsafe { libc::poll(&mut poll, 1, 1000) } {
            0 => return Err(HandoffError::Timeout),
            1 if poll.revents & libc::POLLIN != 0 => (),
            _ => return Err(HandoffError::Transport),
        }
        let mut frame = [0u8; FRAME_BYTES];
        let mut control = [0usize; 64];
        let packet = read_packet(socket, &mut frame, &mut control)?;
        let peers = credentials(&control, packet.control_bytes)?;
        let mut selected = *domain;
        let result = if let Some(validate) = validate_result {
            let value = frame[15];
            if !validate(value) {
                return Err(HandoffError::Frame);
            }
            selected[15] = value;
            value
        } else {
            0
        };
        verify_domain(
            expected,
            connected,
            &peers,
            &frame[..packet.count.min(FRAME_BYTES)],
            packet.truncated,
            &selected,
        )?;
        Ok(result)
    }

    struct PacketRead {
        count: usize,
        control_bytes: usize,
        truncated: bool,
    }

    fn read_packet(
        socket: &OwnedFd,
        frame: &mut [u8],
        control: &mut [usize],
    ) -> Result<PacketRead, HandoffError> {
        let mut iov = libc::iovec {
            iov_base: frame.as_mut_ptr().cast(),
            iov_len: frame.len(),
        };
        let mut message: libc::msghdr = unsafe { zeroed() };
        message.msg_iov = &mut iov;
        message.msg_iovlen = 1;
        message.msg_control = control.as_mut_ptr().cast();
        message.msg_controllen = size_of_val(control);
        let count = unsafe {
            libc::recvmsg(
                socket.as_raw_fd(),
                &mut message,
                libc::MSG_CMSG_CLOEXEC | libc::MSG_DONTWAIT,
            )
        };
        if count < 0 {
            return Err(HandoffError::Transport);
        }
        Ok(PacketRead {
            count: count as usize,
            control_bytes: message.msg_controllen,
            truncated: message.msg_flags & (libc::MSG_TRUNC | libc::MSG_CTRUNC) != 0,
        })
    }

    #[cfg(test)]
    pub(super) fn receive_request(
        socket: &OwnedFd,
        frame: &mut [u8],
    ) -> Result<usize, HandoffError> {
        let expected = PeerIdentity {
            pid: unsafe { libc::getppid() },
            uid: unsafe { libc::getuid() },
            gid: unsafe { libc::getgid() },
        };
        receive_request_from(socket, frame, expected)
    }

    pub(super) fn receive_request_from(
        socket: &OwnedFd,
        frame: &mut [u8],
        expected: PeerIdentity,
    ) -> Result<usize, HandoffError> {
        if identity(option::<libc::ucred>(
            socket.as_raw_fd(),
            libc::SO_PEERCRED,
        )?) != expected
        {
            return Err(HandoffError::Peer);
        }
        let mut control = [0usize; 64];
        let packet = read_packet(socket, frame, &mut control)?;
        if packet.truncated {
            return Err(HandoffError::Truncated);
        }
        if credentials(&control, packet.control_bytes)? != [expected] {
            return Err(HandoffError::Peer);
        }
        Ok(packet.count)
    }

    pub(super) fn require_eof(socket: &OwnedFd) -> Result<(), HandoffError> {
        let mut extra = [0u8; 1];
        let mut control = [0usize; 64];
        let packet = read_packet(socket, &mut extra, &mut control)?;
        if packet.control_bytes != 0 {
            credentials(&control, packet.control_bytes)?;
        }
        if packet.count != 0 || packet.control_bytes != 0 || packet.truncated {
            return Err(HandoffError::Frame);
        }
        Ok(())
    }

    pub(super) fn receive(socket: OwnedFd, expected: PeerExpectation) -> Result<(), HandoffError> {
        receive_inner(&socket, &expected)
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use std::os::fd::FromRawFd;

        fn pair() -> (OwnedFd, OwnedFd) {
            let mut fds = [-1; 2];
            assert_eq!(
                unsafe {
                    libc::socketpair(
                        libc::AF_UNIX,
                        libc::SOCK_SEQPACKET | libc::SOCK_CLOEXEC,
                        0,
                        fds.as_mut_ptr(),
                    )
                },
                0
            );
            unsafe { (OwnedFd::from_raw_fd(fds[0]), OwnedFd::from_raw_fd(fds[1])) }
        }

        fn arm(receiver: &OwnedFd) {
            let enabled: i32 = 1;
            assert_eq!(
                unsafe {
                    libc::setsockopt(
                        receiver.as_raw_fd(),
                        libc::SOL_SOCKET,
                        libc::SO_PASSCRED,
                        ptr::addr_of!(enabled).cast(),
                        size_of::<i32>() as libc::socklen_t,
                    )
                },
                0
            );
        }

        fn expect_self() -> PeerExpectation {
            PeerExpectation {
                identity: PeerIdentity {
                    pid: unsafe { libc::getpid() },
                    uid: unsafe { libc::getuid() },
                    gid: unsafe { libc::getgid() },
                },
                epoch: [7; 32],
            }
        }

        fn frame() -> [u8; FRAME_BYTES] {
            let mut frame = [7; FRAME_BYTES];
            frame[..16].copy_from_slice(DOMAIN);
            frame
        }

        #[test]
        fn kernel_packet_and_connected_identity_agree() {
            let (receiver, sender) = pair();
            arm(&receiver);
            let expected = expect_self();
            let frame = frame();
            assert_eq!(
                unsafe {
                    libc::send(
                        sender.as_raw_fd(),
                        frame.as_ptr().cast(),
                        frame.len(),
                        libc::MSG_NOSIGNAL,
                    )
                },
                FRAME_BYTES as isize
            );
            assert_eq!(receive(receiver, expected), Ok(()));
        }

        #[test]
        fn absent_credentials_are_rejected() {
            assert_eq!(credentials(&[0usize; 64], 0), Err(HandoffError::Ancillary));
            assert_eq!(
                credentials(&[0usize; 1], size_of::<usize>() + 1),
                Err(HandoffError::Ancillary)
            );
            let (receiver, _sender) = pair();
            assert_eq!(receive(receiver, expect_self()), Err(HandoffError::Socket));
        }

        #[test]
        fn different_process_cannot_use_the_creators_connected_credential() {
            let (receiver, sender) = pair();
            arm(&receiver);
            let expected = expect_self();
            let frame = frame();
            let child = unsafe { libc::fork() };
            assert!(child >= 0);
            if child == 0 {
                let sent = unsafe {
                    libc::send(
                        sender.as_raw_fd(),
                        frame.as_ptr().cast(),
                        frame.len(),
                        libc::MSG_NOSIGNAL,
                    )
                };
                unsafe { libc::_exit(i32::from(sent != FRAME_BYTES as isize)) };
            }
            let result = receive(receiver, expected);
            let mut status = 0;
            assert_eq!(unsafe { libc::waitpid(child, &mut status, 0) }, child);
            assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0);
            assert_eq!(result, Err(HandoffError::Peer));
        }

        #[test]
        fn oversized_kernel_packet_is_rejected_even_with_an_exact_prefix() {
            let (receiver, sender) = pair();
            arm(&receiver);
            let mut overflow = frame().to_vec();
            overflow.push(0);
            assert_eq!(
                unsafe {
                    libc::send(
                        sender.as_raw_fd(),
                        overflow.as_ptr().cast(),
                        overflow.len(),
                        libc::MSG_NOSIGNAL,
                    )
                },
                overflow.len() as isize
            );
            assert_eq!(
                receive(receiver, expect_self()),
                Err(HandoffError::Truncated)
            );
        }

        #[test]
        fn rejected_rights_are_closed() {
            let (receiver, sender) = pair();
            arm(&receiver);
            let sent = std::fs::File::open("/dev/null").unwrap();
            let handle_count = || std::fs::read_dir("/proc/self/fd").unwrap().count();
            let before = handle_count();
            let mut control = [0usize; 8];
            let mut payload = frame();
            let mut iov = libc::iovec {
                iov_base: payload.as_mut_ptr().cast(),
                iov_len: payload.len(),
            };
            let mut message: libc::msghdr = unsafe { zeroed() };
            message.msg_iov = &mut iov;
            message.msg_iovlen = 1;
            message.msg_control = control.as_mut_ptr().cast();
            message.msg_controllen = unsafe { libc::CMSG_SPACE(size_of::<i32>() as u32) } as usize;
            unsafe {
                let header = libc::CMSG_FIRSTHDR(&message);
                assert!(!header.is_null());
                (*header).cmsg_level = libc::SOL_SOCKET;
                (*header).cmsg_type = libc::SCM_RIGHTS;
                (*header).cmsg_len = libc::CMSG_LEN(size_of::<i32>() as u32) as usize;
                libc::CMSG_DATA(header)
                    .cast::<i32>()
                    .write_unaligned(sent.as_raw_fd());
                assert_eq!(
                    libc::sendmsg(sender.as_raw_fd(), &message, libc::MSG_NOSIGNAL),
                    FRAME_BYTES as isize
                );
            }
            assert_eq!(
                receive(receiver, expect_self()),
                Err(HandoffError::Ancillary)
            );
            assert_eq!(handle_count(), before - 1);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fixed_frame_and_both_kernel_identities_are_required() {
        let peer = PeerIdentity {
            pid: 42,
            uid: 1000,
            gid: 1000,
        };
        let expected = PeerExpectation {
            identity: peer,
            epoch: [7; 32],
        };
        let mut frame = [7; FRAME_BYTES];
        frame[..16].copy_from_slice(DOMAIN);
        assert_eq!(verify(&expected, peer, &[peer], &frame, false), Ok(()));
        let other = PeerIdentity { pid: 43, ..peer };
        assert_eq!(
            verify(&expected, other, &[peer], &frame, false),
            Err(HandoffError::Peer)
        );
        assert_eq!(
            verify(&expected, peer, &[other], &frame, false),
            Err(HandoffError::Peer)
        );
        assert_eq!(
            verify(&expected, peer, &[], &frame, false),
            Err(HandoffError::Ancillary)
        );
        assert_eq!(
            verify(&expected, peer, &[peer, peer], &frame, false),
            Err(HandoffError::Ancillary)
        );
        assert_eq!(
            verify(&expected, peer, &[peer], &frame, true),
            Err(HandoffError::Truncated)
        );
        assert_eq!(
            verify(&expected, peer, &[peer], &frame[..47], false),
            Err(HandoffError::Frame)
        );
        frame[0] ^= 1;
        assert_eq!(
            verify(&expected, peer, &[peer], &frame, false),
            Err(HandoffError::Frame)
        );
        frame[0] ^= 1;
        frame[16] ^= 1;
        assert_eq!(
            verify(&expected, peer, &[peer], &frame, false),
            Err(HandoffError::Frame)
        );
    }
}

#[cfg(target_os = "linux")]
pub(in crate::installation) mod launcher;
#[cfg(target_os = "linux")]
pub(in crate::installation) use launcher::{
    ExactExecutable, LaunchAuthority, QuiescentReply, Rendezvous,
};
