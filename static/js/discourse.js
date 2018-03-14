function createDOMElement(opt) {
  var el = document.createElement(opt.element || 'div');
  el.className = opt.className || '';
  el.id = opt.id || '';
  el.href = opt.href || '';
  for (var key in opt.style) {
    el.style[key] = opt.style[key];
  }
  for (var i = 0, l = opt.children ? opt.children.length : 0; i < l; i++) {
    el.appendChild(opt.children[i]);
  }
  if (opt.onClick) {
    el.addEventListener('click', opt.onClick, true);
  }
  return el;
};

function div(opt) {
  return createDOMElement(opt);
};

function br() {
  return createDOMElement({element: 'br'});
}

function a(opt) {
  opt.element = 'a';
  return createDOMElement(opt);
};

function text(text) {
  return document.createTextNode(text);
}

var expanded = false;
var discourseDiv = div({
  className: 'comments',
  id: 'discourse-comments',
  style: {
    position: 'fixed',
    backgroundColor: 'white',
    right: 0,
    top: 0,
    bottom: 0,
    borderLeft: 'solid 1px black',
    fontFamily: '"proxima-nova", "Helvetica Neue", Helvetica, Roboto, Arial, sans-serif',
    height: '100%',
    width: '400px',
    transition: '.4s linear',
    right: '-400px',
    zIndex: 10
  },
  children: [
    div({
      style: {
        fontWeight: '400',
        padding: '10px',
        backgroundColor: '#ffffa7',
        marginBottom: '10px',
        textAlign: 'center',
        fontSize: '14px',
        lineHeight: '18px'
      },
      children: [
          text(
            "Note: Discourse is intended for general discussion of the chapter. " +
            "For other things like typo's we encourage you to make "
          ),
          a({
            href:"https://github.com/realworldocaml/book/issues",
            children: [
              text("an issue on GitHub")
            ]
          }),
          text("."),
          br(),
          br(),
          text("When giving feedback please quote the paragraph.")
      ]
    }),
    div({
      style: {
        position: 'absolute',
        backgroundColor: '#8e44ad',
        color: 'white',
        width: '107px',
        height: '26px',
        left: '-67px',
        top: '46px',
        transform: 'rotate(270deg)',
        textAlign: 'center',

        cursor: 'pointer'
      },
      onClick: function(e) {
        if (!expanded) {
          e.target.parentNode.style.right = 0;
          expanded = true;
        }
        else {
          e.target.parentNode.style.right = '-400px';
          expanded = false;
        }
      },
      children: [
        text("Feedback")
      ]
    })
  ]
});

document.body.appendChild(discourseDiv);


let topic = -1;
var loc = location.pathname;

var urlTopicMap = {
  '/00-prologue.html': 579,
  '/01-guided-tour.html': 580,
  '/02-variables-and-functions.html': 581,
  '/03-lists-and-patterns.html': 582,
  '/04-files-modules-and-programs.html': 583,
  '/05-records.html': 584,
  '/06-guided-tour.html': 585,
  '/07-guided-tour.html': 586,
  '/08-imperative-programming.html': 587,
  '/09-functors.html': 588,
  '/10-first-class-modules.html': 589,
  '/11-objects.html': 590,
  '/12-classes.html': 591,
  '/13-maps-and-hashtables.html': 592,
  '/14-command-line-parsing.html': 593,
  '/15-json.html': 594,
  '/16-parsing-with-ocamllex-and-menhir.html': 595,
  '/17-data-serialization.html': 596,
  '/18-concurrent-programming.html': 597,
  '/19-foreign-function-interface.html': 598,
  '/20-runtime-memory-layout.html': 702,
  '/21-garbage-collector.html': 703,
  '/22-compiler-frontend.html': 704,
  '/23-compiler-backend.html': 705,
  '/faqs.html': 708,
  '': 706,
  '/': 706,
  '/index.html': 706,
  '/install.html': 709,
  '/toc.html': 707
};

console.info(urlTopicMap[loc]);

DiscourseEmbed = { discourseUrl: 'https://discuss.ocaml.org/',
                   topicId: urlTopicMap[loc] };

(function() {

  var DE = window.DiscourseEmbed || {};
  var comments = document.getElementById('discourse-comments');
  var iframe = document.createElement('iframe');

  ['discourseUrl', 'discourseEmbedUrl', 'discourseUserName'].forEach(function(i) {
    if (window[i]) { DE[i] = DE[i] || window[i]; }
  });

  var queryParams = {};

  if (DE.discourseEmbedUrl) {
    if (DE.discourseEmbedUrl.indexOf('/') === 0) {
      console.error("discourseEmbedUrl must be a full URL, not a relative path");
    }

    queryParams.embed_url = encodeURIComponent(DE.discourseEmbedUrl);
  }

  if (DE.discourseUserName) {
    queryParams.discourse_username = DE.discourseUserName;
  }

  if (DE.topicId) {
    queryParams.topic_id = DE.topicId;
  }

  var src = DE.discourseUrl + 'embed/comments';
  var keys = Object.keys(queryParams);
  if (keys.length > 0) {
    src += "?";

    for (var i=0; i<keys.length; i++) {
      if (i > 0) { src += "&"; }

      var k = keys[i];
      src += k + "=" + queryParams[k];
    }
  }

  iframe.src = src;
  iframe.id = 'discourse-embed-frame';
  iframe.width = "100%";
  iframe.frameBorder = "0";
  iframe.scrolling = "no";
  comments.appendChild(iframe);

  // Thanks http://amendsoft-javascript.blogspot.ca/2010/04/find-x-and-y-coordinate-of-html-control.html
  function findPosY(obj)
  {
    var top = 0;
    if(obj.offsetParent)
    {
        while(1)
        {
          top += obj.offsetTop;
          if(!obj.offsetParent)
            break;
          obj = obj.offsetParent;
        }
    }
    else if(obj.y)
    {
        top += obj.y;
    }
    return top;
  }

  function normalizeUrl(url) {
    return url.toLowerCase().replace(/^https?(\:\/\/)?/, '');
  }

  function postMessageReceived(e) {
    if (!e) { return; }
    if (normalizeUrl(DE.discourseUrl).indexOf(normalizeUrl(e.origin)) === -1) { return; }

    if (e.data) {
      if (e.data.type === 'discourse-resize' && e.data.height) {
        iframe.height = '100%';
      }

      if (e.data.type === 'discourse-scroll' && e.data.top) {
        // find iframe offset
        var destY = findPosY(iframe) + e.data.top;
        window.scrollTo(0, destY);
      }
    }
  }
  window.addEventListener('message', postMessageReceived, false);

})();
