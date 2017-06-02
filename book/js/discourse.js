var discourseDiv = document.createElement('div');
discourseDiv.className = 'comments';
discourseDiv.id = 'discourse-comments';
discourseDiv.style.position = 'fixed'
discourseDiv.style.right = 0;
discourseDiv.style.top = 0;
discourseDiv.style.bottom = 0;
discourseDiv.style.borderLeft = 'solid 1px black';
discourseDiv.style.overflow = 'scroll';
discourseDiv.style.height = '100%';
discourseDiv.style.width = '21%';
discourseDiv.style.zIndex = 10;

document.body.appendChild(discourseDiv);

let topic = 13;

var loc = location.pathname;
if (loc === '/00-prologue.html') {
  topic = 14;
}
else if (loc === '/01-guided-tour.html') {
  topic = 15;
}
else if (loc === '/02-variables-and-functions.html') {
  topic = 16;
}

DiscourseEmbed = { discourseUrl: 'http://rwov2.trydiscourse.com/',
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
