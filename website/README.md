This website was created with [Docusaurus](https://docusaurus.io/).

# Table of contents

* [Get Started in 5 Minutes](#get-started-in-5-minutes)
* [Adding Content](#adding-content)
* [Full Documentation](#full-documentation)

# Get started

1. Make sure all the dependencies for the website are installed:

```sh
# Install dependencies
$ yarn
```
2. Run your dev server:

```sh
# Start the site
$ yarn start
```

The dev website will be located at [localhost:3000/seashell][]

# Adding Content

## Adding a new docs page to an existing sidebar

1. Create the doc as a new markdown file in `/docs`, example `docs/newly-created-doc.md`:

```md
---
id: newly-created-doc
title: This Doc Needs To Be Edited
---

My new content here..
```

1. Refer to that doc's ID in an existing sidebar in `website/sidebar.json`:

```javascript
// Add newly-created-doc to the Getting Started category of docs
{
  "docs": {
    "Getting Started": [
      "quick-start",
      "newly-created-doc" // new doc here
    ],
    ...
  },
  ...
}
```

For more information about adding new docs, click [here](https://docusaurus.io/docs/en/navigation)

## Adding items to your site's top navigation bar

1. Add links to docs, custom pages or external links by editing the headerLinks field of `website/siteConfig.js`:

`website/siteConfig.js`
```javascript
{
  headerLinks: [
    ...
    /* you can add docs */
    { doc: 'my-examples', label: 'Examples' },
    /* you can add custom pages */
    { page: 'help', label: 'Help' },
    /* you can add external links */
    { href: 'https://github.com/facebook/Docusaurus', label: 'GitHub' },
    ...
  ],
  ...
}
```

For more information about the navigation bar, click [here](https://docusaurus.io/docs/en/navigation)

# Full Documentation

Full documentation can be found on the [website](https://docusaurus.io/).

# Deployment

The website is automatically deployed by hooknook githooks.
