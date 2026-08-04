#[cfg(test)]
pub mod tests {
    use std::sync::Mutex;
    pub static TEST_MUTEX: Mutex<()> = Mutex::new(());
}
