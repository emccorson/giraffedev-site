const BUILD_DIR = "docs";
const POSTS_DIR = "posts";

// IMPORTS
const fs = require("fs");
const { titleToUrl, mostRecentDateSort } = require("./utils.js");
const defaultTemplate = require("./templates/default.js");
const postTemplates =
  fs.readdirSync(POSTS_DIR).map(f => require(`./${POSTS_DIR}/${f}`));
const postWrapperTemplate = require("./templates/post.js");

// MAIN
(function cleanBuildDir() {
  fs.rmSync(BUILD_DIR, { recursive: true, force: true });
  fs.mkdirSync(BUILD_DIR);
}());

(function copyCss() {
  fs.cpSync("css", `${BUILD_DIR}/css`, { recursive: true });
}());

(function buildContactPage() {
  const contactTemplate = require("./templates/contact.js");
  const contact = defaultTemplate.html(
    contactTemplate.title,
    contactTemplate.html()
  );
  fs.writeFileSync(`${BUILD_DIR}/contact.html`, contact);
}());

(function buildPosts() {
  postTemplates.forEach(postTemplate => {
    const post = defaultTemplate.html(
      postTemplate.title,
      postWrapperTemplate.html(
        postTemplate.date,
        postTemplate.html()
      )
    );
    const filename = titleToUrl(postTemplate.title);
    fs.writeFileSync(`${BUILD_DIR}/${filename}.html`, post);
  });
}());

(function buildArchivePage() {
  const archiveTemplate = require("./templates/archive.js");
  const archive = defaultTemplate.html(
    "archive",
    archiveTemplate.html(postTemplates)
  );
  fs.writeFileSync(`${BUILD_DIR}/archive.html`, archive);
}());

(function buildHomePage() {
  const homeTemplate = require("./templates/home.js");
  const latestPost = postTemplates.toSorted(mostRecentDateSort)[0];
  const home = defaultTemplate.html(
    latestPost.title,
    homeTemplate.html(
      postWrapperTemplate.html(
        latestPost.date,
        latestPost.html()
      )
    )
  );
  fs.writeFileSync(`${BUILD_DIR}/index.html`, home);
}());
