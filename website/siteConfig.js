const baseUrl = '/fuse/'
const siteConfig = {
  title: 'Fuse',
  tagline: 'A typed programming language for safe high-level synthesis',
  url: 'https://capra.cs.cornell.edu',
  baseUrl: baseUrl,
  projectName: 'fuse',
  organizationName: 'cucapra',
  headerLinks: [
    {doc: 'installation', label: 'Docs'},
    {label: 'Notes', href: 'https://capra.cs.cornell.edu/seashell/docs'},
    {label: 'Github', href: 'https://github.com/cucapra/seashell'},
    {search: true}
  ],

  /* path to images for header/footer */
  headerIcon: '',
  footerIcon: '',
  favicon: '',

  /* Colors for website */
  colors: {
    primaryColor: '#009292',
    secondaryColor: '#009292',
  },

  // This copyright info is used in /core/Footer.js and blog RSS/Atom feeds.
  copyright: `Copyright © ${new Date().getFullYear()} Cornell University`,

  highlight: {
    // Highlight.js theme to use for syntax highlighting in code blocks.
    theme: 'default',
    hljs: function(hljs) {
      hljs.registerLanguage('fuse', function(hljs) {
        return {
          case_insensitive: false,
          keywords: {
            keyword: 'for if while let decl view def bank unroll combine shrink',
            types: "bool bit float"
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

  // Enable search powered by Aloglia
  algolia: {
    apiKey: '988f78fca045e0c0158986e788012d9c',
    indexName: 'cornell_fuse',
  }

};

module.exports = siteConfig;
