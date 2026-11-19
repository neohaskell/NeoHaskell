use super::*;

const CHALLENGE: &[u8; 16] = b"NEO-NIX-CHAL-V01";
const COMMIT: &[u8; 16] = b"NEO-NIX-CMIT-V01";
const BASE_REPLY: &[u8; 16] = b"NEO-POLICY-V001\0";
const COMMIT_BYTES: usize = 112;

pub(super) struct PublishChallenge {
    attempt: [u8; 32],
    reviewed: ReviewedPolicyRequest,
    nonce: [u8; 32],
}

pub(super) enum PublishResponse {
    Challenge(PublishChallenge),
    Outcome(PolicyResult),
}

impl PublishChallenge {
    pub(super) fn new(
        attempt: [u8; 32],
        reviewed: ReviewedPolicyRequest,
    ) -> Result<Self, HandoffError> {
        Ok(Self {
            attempt,
            reviewed,
            nonce: random_token(libc::GRND_NONBLOCK)?,
        })
    }

    fn encode(&self, domain: &[u8; 16]) -> [u8; COMMIT_BYTES] {
        let mut frame = [0; COMMIT_BYTES];
        frame[..16].copy_from_slice(domain);
        frame[16..48].copy_from_slice(&self.attempt);
        frame[48..80].copy_from_slice(self.reviewed.bytes());
        frame[80..].copy_from_slice(&self.nonce);
        frame
    }

    pub(super) fn require_commit(
        self,
        socket: &OwnedFd,
        caller: PeerIdentity,
        deadline: Instant,
    ) -> Result<(), HandoffError> {
        send_packet(socket, &self.encode(CHALLENGE))?;
        let (frame, count) = receive_packet(socket, caller, deadline)?;
        if count != COMMIT_BYTES || frame != self.encode(COMMIT) {
            return Err(HandoffError::Frame);
        }
        Ok(())
    }

    pub(super) fn send_commit(self, socket: &OwnedFd) -> Result<(), HandoffError> {
        send_packet(socket, &self.encode(COMMIT))
    }
}

impl PublishResponse {
    pub(super) fn send_outcome(
        socket: &OwnedFd,
        epoch: &[u8; 32],
        result: PolicyResult,
    ) -> Result<(), HandoffError> {
        let mut frame = [0; super::super::FRAME_BYTES];
        frame[..16].copy_from_slice(BASE_REPLY);
        frame[15] = result as u8;
        frame[16..].copy_from_slice(epoch);
        send_packet(socket, &frame)
    }

    pub(super) fn receive(
        socket: &OwnedFd,
        expected: &PeerExpectation,
        reviewed: ReviewedPolicyRequest,
        deadline: Instant,
    ) -> Result<Self, HandoffError> {
        let (frame, count) = receive_packet(socket, expected.identity, deadline)?;
        if count == super::super::FRAME_BYTES
            && frame[..15] == BASE_REPLY[..15]
            && frame[16..count] == expected.epoch
        {
            let result = PolicyResult::decode(frame[15]).ok_or(HandoffError::Frame)?;
            return Ok(Self::Outcome(result));
        }
        if count != COMMIT_BYTES
            || &frame[..16] != CHALLENGE
            || frame[16..48] != expected.epoch
            || frame[48..80] != *reviewed.bytes()
            || frame[80..] == [0; 32]
        {
            return Err(HandoffError::Frame);
        }
        Ok(Self::Challenge(PublishChallenge {
            attempt: expected.epoch,
            reviewed,
            nonce: frame[80..].try_into().unwrap(),
        }))
    }
}

fn send_packet(socket: &OwnedFd, frame: &[u8]) -> Result<(), HandoffError> {
    let sent = unsafe {
        libc::send(
            socket.as_raw_fd(),
            frame.as_ptr().cast(),
            frame.len(),
            libc::MSG_NOSIGNAL | libc::MSG_DONTWAIT,
        )
    };
    if sent != frame.len() as isize {
        return Err(HandoffError::Transport);
    }
    Ok(())
}

fn receive_packet(
    socket: &OwnedFd,
    peer: PeerIdentity,
    deadline: Instant,
) -> Result<([u8; COMMIT_BYTES], usize), HandoffError> {
    let remaining = deadline.saturating_duration_since(Instant::now());
    if remaining.is_zero() {
        return Err(HandoffError::Timeout);
    }
    let mut poll = libc::pollfd {
        fd: socket.as_raw_fd(),
        events: libc::POLLIN,
        revents: 0,
    };
    match unsafe {
        libc::poll(
            &mut poll,
            1,
            remaining.as_millis().clamp(1, i32::MAX as u128) as i32,
        )
    } {
        0 => return Err(HandoffError::Timeout),
        1 if poll.revents & libc::POLLIN != 0 => (),
        _ => return Err(HandoffError::Transport),
    }
    let mut frame = [0; COMMIT_BYTES];
    let count = linux::receive_request_from(socket, &mut frame, peer)?;
    Ok((frame, count))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn connection(epoch: [u8; 32]) -> (OwnedFd, OwnedFd, PeerExpectation) {
        let mut ends = [-1; 2];
        assert_eq!(
            unsafe {
                libc::socketpair(
                    libc::AF_UNIX,
                    libc::SOCK_SEQPACKET | libc::SOCK_CLOEXEC | libc::SOCK_NONBLOCK,
                    0,
                    ends.as_mut_ptr(),
                )
            },
            0
        );
        let [root, frontend] = ends.map(|fd| unsafe { OwnedFd::from_raw_fd(fd) });
        let enabled: i32 = 1;
        for endpoint in [&root, &frontend] {
            assert_eq!(
                unsafe {
                    libc::setsockopt(
                        endpoint.as_raw_fd(),
                        libc::SOL_SOCKET,
                        libc::SO_PASSCRED,
                        (&enabled as *const i32).cast(),
                        size_of::<i32>() as libc::socklen_t,
                    )
                },
                0
            );
        }
        let identity = linux::connected_identity(&frontend).unwrap();
        (root, frontend, PeerExpectation { identity, epoch })
    }

    #[test]
    fn policy_commit_requires_one_current_authenticated_response() {
        let epoch = [1; 32];
        let reviewed = ReviewedPolicyRequest::from_bytes([2; 32]);
        let challenge = PublishChallenge::new(epoch, reviewed).unwrap();
        let committed = challenge.encode(COMMIT);
        let (root, frontend, expected) = connection(epoch);
        let caller = expected.identity;
        let deadline = Instant::now() + Duration::from_secs(3);

        std::thread::scope(|scope| {
            let publisher = scope.spawn(move || challenge.require_commit(&root, caller, deadline));
            let PublishResponse::Challenge(challenge) =
                PublishResponse::receive(&frontend, &expected, reviewed, deadline).unwrap()
            else {
                panic!("commit challenge expected");
            };
            challenge.send_commit(&frontend).unwrap();
            publisher.join().unwrap().unwrap();
        });

        let (root, frontend, expected) = connection(epoch);
        let challenge = PublishChallenge::new(epoch, reviewed).unwrap();
        assert!(committed != challenge.encode(COMMIT));
        send_packet(&frontend, &committed).unwrap();
        assert_eq!(
            challenge.require_commit(
                &root,
                expected.identity,
                Instant::now() + Duration::from_secs(3)
            ),
            Err(HandoffError::Frame)
        );
    }

    #[test]
    fn policy_commit_refuses_absence_and_mismatched_frames() {
        let epoch = [1; 32];
        let reviewed = ReviewedPolicyRequest::from_bytes([2; 32]);
        for byte in [0, 16, 48, 80] {
            let (root, frontend, expected) = connection(epoch);
            let challenge = PublishChallenge::new(epoch, reviewed).unwrap();
            let mut wrong = challenge.encode(COMMIT);
            wrong[byte] ^= 1;
            send_packet(&frontend, &wrong).unwrap();
            assert_eq!(
                challenge.require_commit(
                    &root,
                    expected.identity,
                    Instant::now() + Duration::from_secs(3)
                ),
                Err(HandoffError::Frame)
            );
        }

        let (root, frontend, expected) = connection(epoch);
        let challenge = PublishChallenge::new(epoch, reviewed).unwrap();
        drop(frontend);
        assert!(
            challenge
                .require_commit(
                    &root,
                    expected.identity,
                    Instant::now() + Duration::from_secs(3)
                )
                .is_err()
        );

        let (root, _frontend, expected) = connection(epoch);
        let challenge = PublishChallenge::new(epoch, reviewed).unwrap();
        assert_eq!(
            challenge.require_commit(
                &root,
                expected.identity,
                Instant::now() + Duration::from_millis(25)
            ),
            Err(HandoffError::Timeout)
        );
    }
}
