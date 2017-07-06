function createElement(opt) {
  var el = document.createElement(opt.element || 'div');
  el.className = opt.className || '';
  el.id = opt.id;
  for (var key in opt.style) {
    el.style[key] = opt.style[key];
  }
  return el;
};

var discourseDiv = createElement({
  className: 'comments',
  id: 'discourse-comments',
  style: {
    position: 'fixed',
    right: 0,
    top: 0,
    bottom: 0,
    borderLeft: 'solid 1px black',
    overflow: 'scroll',
    height: '100%',
    width: '21%',
    zIndex: 10,
    position: 'relative'
  }
});
//
// document.createElement('div');
// discourseDiv.className = 'comments';
// discourseDiv.id = 'discourse-comments';
//
//
// var someDiv = document.createElement

document.body.appendChild(discourseDiv);

let topic = -1;

var loc = location.pathname;
if (loc === '/01-guided-tour.html') {
  topic = 498;
}

DiscourseEmbed = { discourseUrl: 'http://discuss.ocaml.org/',
                   topicId: topic };

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
