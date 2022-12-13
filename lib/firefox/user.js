// user.js --- firefox customisations

// Fixes auto-activation of the topmost option in the right-click
// menus.
//
// cf: https://github.com/qtile/qtile/issues/2460
//     https://bugzilla.mozilla.org/show_bug.cgi?id=1355848
user_pref("ui.context_menus.after_mouseup", true);

// Allows using ‘userChrome.css’.
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);

// Cf. https://bugzilla.mozilla.org/show_bug.cgi?id=1749908
user_pref("network.http.http3.enabled", false);

// Open new tab next to current tab.
user_pref("browser.tabs.insertAfterCurrent", true);

// Add this option to make =org-protocol= work properly again.
// cf. https://bugzilla.mozilla.org/show_bug.cgi?id=1685682
user_pref("network.protocol-handler.external.org-protocol", true);

// Disable pocket because who tf cares about pocket besides wankers
// over at Mozilla?
user_pref("extensions.pocket.enabled", false);

// Do not auto hide scrollbars, because why the heck??
// cf. https://todon.eu/@maxi/108356307151802140
user_pref("widget.gtk.overlay-scrollbars.enabled", false);

// Leave my tabs alone, do not unloat automatically.
// cf. https://firefox-source-docs.mozilla.org/browser/tabunloader/
user_pref("browser.tabs.unloadOnLowMemory", false);

// Nicer scrolling.

// Pixelwise scroll; by default firefox scrolls linewise and it's
// janky and inaccurate, terrible, in usual firefox fashion... this
// smoothens it a bit. Smaller values make scrolling painfully slow
// unless the next two settings are set
//
// These settings assume the env var MOZ_USE_XINPUT2 is set to "1".
user_pref("mousewheel.default.delta_multiplier_y", 1);
// Accelerate scrolling quicker.
user_pref("mousewheel.acceleration.factor", 100);
// Enable scroll acceleration; the higher the number the higher the
// inertia for acceleration to kick in.
user_pref("mousewheel.acceleration.start", 0);

// Let proxies be.
// viz. https://support.mozilla.org/en-US/questions/926378
user_pref("network.automatic-ntlm-auth.allow-proxies", false);
user_pref("network.negotiate-auth.allow-proxies", false);

// Do not necessarily use the dark theme in private windows (new
// «feature» in Firefox 106).
user_pref("browser.theme.dark-private-windows", false);

// Pick up dark theme from GTK.
user_pref("widget.content.allow-gtk-dark-theme", true);
