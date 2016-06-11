// no-script fallback
console.log("--- here in download.js -- new version")

var preferredOS = "osx";

$(document).ready(function() {
    $('body').addClass('js');
    // display or hide certain areas
});

function identify_platform() {
    var ua = navigator.userAgent;
    var userAgents = {
        'Mac OSX': 'osx',
        'Mac OS X': 'osx',
        'Linux': 'linux',
        'Windows': 'windows'
    };

    if (preferredOS != "") {
      return preferredOS;
    }

    if (ua.indexOf('Android') != -1)
        return 'unknown';

    for (key in userAgents) {
        if (ua.indexOf(key) != -1) {
            return userAgents[key];
        }
    }
    return 'unknown';
}

var platformNames = {
    'osx': 'Mac OS X',
    'linux': 'Linux',
    'windows': 'Microsoft Windows',
};

// Infer user's operating system
$(document).ready(function() {
    var platform = identify_platform();
    if (platform != 'unknown'){
        var $platform = $(".downloads-platform[data-os='"+platform+"']");
        $platform
            .prependTo('#platforms')
            .addClass('preferred-platform')
            .addClass('visible');
        console.log("added visible for:", platform)

        $(".found-user-platform strong").text(platformNames[platform]);
        $("body").addClass('user-platform-known');
        // ...
        // $('.downloads-platform').removeClass('visible');
        // $("section").find("[data-os='linux']").addClass('visible')
        // 
        console.log("platform:", platform)
    }
    $('.downloads-platform').removeClass('visible');
    $(".preferred-platform").removeClass("preferred-platform")
    console.log("removed preferred-platform and visible from downloads-platform")
});

// When an expander is clicked, hide the current expander
// and make the clicked expander visible.
$(document).ready(function() {
    $('a.expander').click(function() {
        var $this = $(this);
        $('.downloads-platform').removeClass('visible');
        $this.parents('.downloads-platform').addClass('visible');

        var ypos = $this.parents("section").offset().top
        console.log("--- ypos:", ypos)

        // $('html, body').animate({ scrollTop: 0 + 'px' }, 'fast');

    });
});

// When clicking on SHA hash text fields, select the entire field
// to faciliate copying the value to the clipboard.
$(document).ready(function() {
    $('.hashes .file-hash').each(function() {
        $(this).click(function() {this.select();})
    });
});

// Linux flavors
if (0) {
$(document).ready(function() {
    $('#linux-prompt').addClass('active');
});
} else { console.log("not activating linux prompt") }

// Operating system flavors
$(document).ready(function() {
    $('.flavors li a').click(function(event) {
        event.preventDefault();
        var $this = $(this);
        var distro = $this.attr('href');
        var $platform = $this.parents('.downloads-platform');

        $this.parents('.sidebar').addClass('chosen');
        $this.parents('ul').children('li').removeClass('active');
        $this.parents('li').addClass('active');

        $platform.find('.flavor').removeClass('active');
        $(distro).addClass('active');

        $('html, body').scrollTop($platform.offset().top);
        window.history.replaceState({}, '', distro);
    });
});
