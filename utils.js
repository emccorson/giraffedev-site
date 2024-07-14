const titleToUrl = (title) => title
  .toLowerCase()
  .replaceAll(" ", "-")
  .replace(/[^\w-]/g, "");

const mostRecentDateSort = (a, b) => new Date(b.date) - new Date(a.date);

module.exports = {
  titleToUrl,
  mostRecentDateSort,
};
