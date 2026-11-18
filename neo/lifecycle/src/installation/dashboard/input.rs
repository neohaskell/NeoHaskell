use std::time::{Duration, Instant};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Key {
    Accept,
    Details,
    Back,
    Up,
    Down,
    Cancel,
}

enum Sequence {
    Ground,
    Escape,
    Control(Vec<u8>),
    Paste { matched: usize, bytes: usize },
    Utf8(u8),
    Disabled,
}

pub(super) struct Input {
    sequence: Sequence,
    deadline: Option<Instant>,
    generation: u64,
}

impl Input {
    pub(super) fn new() -> Self {
        Self {
            sequence: Sequence::Ground,
            deadline: None,
            generation: 0,
        }
    }
    pub(super) fn reset(&mut self) {
        if !matches!(self.sequence, Sequence::Disabled) {
            self.sequence = Sequence::Ground;
        }
        self.deadline = None;
        self.generation = self.generation.wrapping_add(1);
    }
    pub(super) fn generation(&self) -> u64 {
        self.generation
    }
    pub(super) fn deadline(&self) -> Option<Instant> {
        self.deadline
    }
    pub(super) fn expire(&mut self, now: Instant) -> Option<Key> {
        if self.deadline.is_some_and(|deadline| now >= deadline) {
            self.deadline = None;
            if matches!(self.sequence, Sequence::Escape) {
                self.sequence = Sequence::Ground;
                return Some(Key::Back);
            }
            self.disable();
            return Some(Key::Cancel);
        }
        None
    }
    fn disable(&mut self) {
        self.sequence = Sequence::Disabled;
        self.deadline = None;
        self.generation = self.generation.wrapping_add(1);
    }
    pub(super) fn decode(&mut self, bytes: &[u8], now: Instant) -> Vec<Key> {
        let mut keys: Vec<_> = self.expire(now).into_iter().collect();
        for &byte in bytes {
            if matches!(self.sequence, Sequence::Disabled) || keys.len() >= 32 {
                self.disable();
                return vec![Key::Cancel];
            }
            if byte == 3 || byte == 26 {
                keys.push(Key::Cancel);
                continue;
            }
            match &mut self.sequence {
                Sequence::Ground => match byte {
                    b'\r' | b'\n' => keys.push(Key::Accept),
                    b'd' | b'D' => keys.push(Key::Details),
                    b'q' | b'Q' => keys.push(Key::Cancel),
                    27 => {
                        self.sequence = Sequence::Escape;
                        self.deadline = Some(now + Duration::from_millis(100));
                    }
                    0xc2..=0xdf => self.sequence = Sequence::Utf8(1),
                    0xe0..=0xef => self.sequence = Sequence::Utf8(2),
                    0xf0..=0xf4 => self.sequence = Sequence::Utf8(3),
                    _ => {}
                },
                Sequence::Escape => {
                    self.sequence = if byte == b'[' || byte == b'O' {
                        Sequence::Control(Vec::new())
                    } else {
                        keys.push(Key::Cancel);
                        Sequence::Disabled
                    };
                }
                Sequence::Control(sequence) => {
                    if sequence.len() >= 64 || !(0x20..=0x7e).contains(&byte) {
                        self.disable();
                        return vec![Key::Cancel];
                    }
                    sequence.push(byte);
                    if (0x40..=0x7e).contains(&byte) {
                        if sequence == b"200~" {
                            self.sequence = Sequence::Paste {
                                matched: 0,
                                bytes: 0,
                            };
                            self.deadline = Some(now + Duration::from_secs(2));
                            continue;
                        }
                        match sequence.as_slice() {
                            b"A" => keys.push(Key::Up),
                            b"B" => keys.push(Key::Down),
                            _ => {}
                        }
                        self.sequence = Sequence::Ground;
                        self.deadline = None;
                    }
                }
                Sequence::Paste { matched, bytes } => {
                    *bytes += 1;
                    if *bytes > 4096 {
                        self.disable();
                        return vec![Key::Cancel];
                    }
                    let ending = b"\x1b[201~";
                    *matched = if byte == ending[*matched] {
                        *matched + 1
                    } else {
                        usize::from(byte == 27)
                    };
                    if *matched == ending.len() {
                        self.sequence = Sequence::Ground;
                        self.deadline = None;
                    }
                }
                Sequence::Utf8(remaining) => {
                    if (0x80..=0xbf).contains(&byte) {
                        *remaining -= 1;
                        if *remaining == 0 {
                            self.sequence = Sequence::Ground;
                        }
                    } else {
                        self.disable();
                        return vec![Key::Cancel];
                    }
                }
                Sequence::Disabled => return vec![Key::Cancel],
            }
        }
        keys
    }
}
