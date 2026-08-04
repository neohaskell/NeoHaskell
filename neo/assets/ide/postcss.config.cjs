// Mantine's PostCSS pipeline. `postcss-preset-mantine` provides the
// light-dark() helper + rem() etc.; `postcss-simple-vars` exposes the
// breakpoint variables Mantine references in its CSS.
module.exports = {
  plugins: {
    'postcss-preset-mantine': {},
    'postcss-simple-vars': {
      variables: {
        'mantine-breakpoint-xs': '36em',
        'mantine-breakpoint-sm': '48em',
        'mantine-breakpoint-md': '62em',
        'mantine-breakpoint-lg': '75em',
        'mantine-breakpoint-xl': '88em',
      },
    },
  },
}
