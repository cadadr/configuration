// w0.js --- decode wikizero links

// Wikizero is a proxy to Wikipedia.  They encode links in pages to
// base64 urls that they redirect.  This script retrieves such links.
// useful for printing an article (to PDF or not), or to skip their
// proxy for non-Wikipedia links.

(function () {
  var errs = {};
  document.querySelectorAll('a').forEach(
    (elem) => {
      try {
        elem.href = atob(elem.href.split(/\?q=/)[1]);
      } catch (e) {
        errs[elem.href] = e;
      };
      if(errs.length > 0) {
        console.log("wikizero errs: %d errs encountered", errs.length);
        window.w0_errs = errs;
      }
    });
})();

// Bookmarklet version:
// javascript:(function () { var errs = {}; document.querySelectorAll('a').forEach( (elem) => { try { elem.href = atob(elem.href.split(/\?q=/)[1]); } catch (e) { errs[elem.href] = e; }; if(errs.length > 0) { console.log("wikizero errs: %d errs encountered", errs.length); window.w0_errs = errs; } }); })();
