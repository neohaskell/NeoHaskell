pub(crate) struct BuildIdentity {
    pub(crate) product_version: &'static str,
    pub(crate) source_revision: &'static str,
    pub(crate) target: &'static str,
}

impl BuildIdentity {
    pub(crate) fn current() -> Self {
        Self {
            product_version: env!("NEO_PRODUCT_VERSION"),
            source_revision: env!("NEO_SOURCE_REVISION"),
            target: env!("NEO_BUILD_TARGET"),
        }
    }
}
