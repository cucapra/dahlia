const baseUrl = '/fuse/'
const siteConfig = {
  title: 'Dahlia',
  tagline: 'A typed programming language for safe high-level synthesis',
  url: 'https://capra.cs.cornell.edu',
  baseUrl: baseUrl,
  projectName: 'fuse',
  organizationName: 'cucapra',
  headerLinks: [
    {doc: 'installation', label: 'Docs'},
    {label: 'Notes', href: 'https://capra.cs.cornell.edu/seashell/docs'},
    {label: 'GitHub', href: 'https://github.com/cucapra/seashell'}
  ],

  /* path to images for header/footer */
  headerIcon: '',
  footerIcon: '',
  favicon: '',

  /* Colors for website */
  colors: {
    primaryColor: '#b31b1b',  // Cornell red.
    secondaryColor: '#541b18',
  },

  // This copyright info is used in /core/Footer.js and blog RSS/Atom feeds.
  copyright: `Copyright © ${new Date().getFullYear()} Cornell University`,

  highlight: {
    // Highlight.js theme to use for syntax highlighting in code blocks.
    theme: 'default',
    hljs: function(hljs) {
      hljs.registerLanguage('dahlia', function(hljs) {
        return {
          case_insensitive: false,
          keywords: {
            title: 'for if while let decl view def record import',
            symbol: 'bank unroll combine shrink',
            // add point and rect for nicer highlighting even though they aren't primitive
            type: "bool bit float point rect",
            literal: "true false"
          },
          contains: [
            hljs.C_LINE_COMMENT_MODE,
            hljs.C_BLOCK_COMMENT_MODE,
            hljs.C_NUMBER_MODE
          ]
        }
      })
    }
  },

  // Add custom scripts here that would be placed in <script> tags.
  scripts: [
    'https://buttons.github.io/buttons.js',
    'https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.0/clipboard.min.js',
    baseUrl + 'js/code-block-buttons.js'
  ],

  stylesheets: [baseUrl + 'css/code-block-buttons.css'],

  // On page navigation for the current documentation page.
  onPageNav: 'separate',
  // No .html extensions for paths.
  cleanUrl: true,

  // Show documentation's last contributor's name.
  enableUpdateBy: true,

  // Show documentation's last update time.
  enableUpdateTime: true,

};

module.exports = siteConfig;
