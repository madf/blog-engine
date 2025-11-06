# General

## The goal

Create a blog engine which can create static blogs (pure html), supports online editing and allows uploading images.

Public part is static HTML, administrative part (aka blog editor) is server-rendered plus API.

## The stack

 * Haskell as a language (because it is fun);
 * Web.Scotty as a framework (might change it to Servant) because I am familiar with it and it can serve both files and API;
 * SQLite as a database (better than plain files, easier to manage than the other RDBMS);
 * filesystem as a storage for static data (rendered pages and images);
 * Nginx as a front-end to add SSL, caching, CSP, etc.

## Storage format

A post is a list of blocks. Each block has a type. The simplest block is a text block which represnts a paragraph of text. The other type is a carousel block, which represents a carousel of images. There might be other type, for example a code block with code highlighting, but they are not implemented yet. Blocks are rendered in the same order as they are stored. Carousels are rendered with image previews, smaller versions of the real images. Each preview is a link to the real image.

A post can have 3 types of visibility:
 * public - posts are rendered in static HTML and are linked from indices;
 * unlisted - posts are rendered in static HTML, but no links are provided (they can only be accessed by provate links);
 * private - posts are NOT rendered in static HTML and are only available from the blog editor.

Unlisted and private posts have a 'Reason' field, where it is possible to state the reason behind this visibility level. E.g. 'personal', 'family-related', 'do not want my boss see it', etc.

## Slugs

Since simple post ids can be enumerated and queried, it is not reasonable to use them with unlisted posts. Someone can query ids one by one to discover these unlisted posts. That's why the slugs are used instead. A slug is a randomly generated alphanumeric string followed by the post id to counter possible collisions. The same slug is used for the post HTML file and for the post's image directory.

## Images

Each image in a carousel block have a title and two files: one with the original image and the other with a scaled down preview. All previews are scaled to the same height. All images are related to a post and their files are stored in a dedicated directory.

Images within a post are deduplicated by hash. When an image is uploaded, at first we calculate a hash from the image data. And if there is the same hash among the other post images, no uploading is performed and only a ref counter is increased. If there is no such hash, the image data is store in a file in the dedicated directory. Then a scaled down preview is generated. A metadata from both the original image and the preview is then store in the database. When an image is deleted, its ref counter is decreasing. When it reached zero, both files (the image and the preview) are deleted and the database entry is deleted.

Hash-based deduplication and ref counting is used to allow a user to upload the same image in different carousel blocks without necessity to generate filenames to prevent file collisions (TODO: check file collisions from different images). With the current approach filenames are preserved. A downside: the same image within a post can have only one title, one can not put different titles to the same image.

Image duplicates in different posts are permitted.

## File structure

Public part file structure:
```
 / -+- index.html
    +- <year> -+- index.html
               +- <slug>-<id>.html
               +- <slug>-<id> -+- <image>.jpg
                               +- preview-<image>.jpg
```

## Main and year contents

Main `index.html` is a blog front page with a number of post exeprts, rendered from the most recent on top. A post exerpt contains a post date, post title, first image from the first carousel block, if any and a part of the first text block, if any. The date and the title (maybe the whole exerpt?) is a link to the post itself. At the bottom of the main page there is a link to the current year index.

Year `index.html` follows the same scheme, but contains all the posts of the year.

## Post page

The post page starts from the post date and title, with the blocks following below.

# Administrative part (blog editor)

## Authorization

The system has a single user which is authenticated by login and password. Upon successfull authentication, a JWT token is issued for accessing admin pages and API. For the pages the token is stored in a dedicated cookie, and for the API access the token is provided in the request 'Authorization' header.

Login and password are stored in the configuration file. The login is stored in clear text and the password is stored as an Argon2 hash.

Token signing and verification is preformed by an auto-generated key (or key pair) which is stored in memory. Optionally it can be stored permanently in a file to persist between system restarts, but I don't know if it makes any sense.

## Pages

The administrative part consists of the following pages:
 * login page;
 * index page;
 * year index page;
 * post preview page;
 * post edit page;
 * new post pseudo-page.

The login page is a simple login/password form. Unauthorized users are redirected to the login page if they try to access admin pages. Upon a successfull login, the user is redirected back to the original page, or to the index page.

The index page shows a list of recent posts, ordered from the most recent to the least recent. Each post is represented by an exerpt which contains the date, the title, the first picture from the first carousel block and a shortened first block of text. The link from the each exerpt goes to the preview page. There is also a dedicated button that redirects to the post edit page. This index supports pagination by query params 'page' and 'perPage'. If not specified, they default to page 0 and 10 posts per page.

Same applies to the year index.

The preview page shows the post in the same way as it is rendered (or would be rendered, for private posts) in the static public part, except that it has an 'Edit' button which redirects to the edit page.

The edit page represents each block as an editable widget. Text blocks are represented by textareas, and carousels are represented by special image uploading widget. Each block can be deleted or moved up and down in the block list. Each image in the carousel has an editable title and buttons to delete it or move left or right inside the carousel. A user can add a new text block, a new carousel block or a new image in an existing carousel block. TODO: change it to a single div with contenteditable.

The 'new post' pseudo-page is just an endpoint which creates an empty post entry and redirects user to the edit page for this new entry.

## API

### Login API

Two endpoints:
 * issue a token provided login and password, returns the token;
 * renew a token provided the current token, returns the new token.

### Post API

Four endpoints:
 * get a post by a slug, returns the post;
 * update a post by a slug, returns nothing, optionally renders the post and regenerates the index, and years;
 * add an image to a post, identified by a slug, returns the image metadata;
 * re-render all generated files.

### Image API

Three endpoints:
 * get an image by id, returns the image metadata;
 * change a title of an image, identified by id, returns nothing;
 * delete an image by id (decrease ref counter and delete when it reaches zero), returns nothing.
