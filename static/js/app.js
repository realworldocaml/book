$(document).foundation();

var isScrolled = false, $sn = $('.left-column'), snTop, snInHeight, snScroll = 0;
var $snSide = $('.main-body'), wS = 0, wD = true, ww = $(window).width(), wh = $(window).height();

function checkScroll() {
	if (isScrolled)
	{
		if ($sn)
		{
			if (snTop < wS && $snSide.offset().top + $snSide.outerHeight() > wS + wh)
			{
				$sn.css('min-height', '');
				if (!$sn.hasClass('true-fixed'))
				{
					$sn.addClass('true-fixed').removeClass('top bottom');
				}

				if ($sn.scrollTop() > wS - $snSide.offset().top && !wD)
				{
					snScroll = wS - $snSide.offset().top;
				}
				else if ($sn.scrollTop() < wS - $snSide.offset().top - $snSide.outerHeight() + snInHeight && wD)
				{
					snScroll = wS - $snSide.offset().top - $snSide.outerHeight() + snInHeight;
				}
				$sn.scrollTop(snScroll);

			}
			else
			{
				$sn.removeClass('true-fixed');

				if ($snSide.offset().top + $snSide.outerHeight() > wS + wh)
				{
					$sn.addClass('top').removeClass('bottom');
					snScroll = 0;
				}
				else
				{
					$sn.addClass('bottom').removeClass('top');
					$sn.css('min-height', wh);
					snScroll = snInHeight - $(window).height();
				}

			}
		}
		isScrolled = false;
	}
	window.requestAnimationFrame(checkScroll);
}

$(document).ready(function(){
	if ($('section').length > 0)
	{
		$('section').scrollNav({
			sections:		'section>h2',
			subSections:	'section',
			sectionElem:	'div',
			insertTarget:	$('.left-column').get(0),
			insertLocation:	'prependTo',
			fixedMargin:	1000,
			showHeadline:	false,
			scrollToHash:	false,
			showTopLink:	false
		});
	}

	if ($('.toc').length > 0)
	{
		$('.toc .children').slideUp(0).before($('<i>+</i>'));
		$('.toc i').on('click', function(e){
			e.preventDefault();
			e.stopPropagation();
			if (e.altKey || e.ctrlKey || e.metaKey)
			{
				if ($(this).hasClass('on'))
				{
					$('.toc i').removeClass('on').next().slideUp();
				}
				else
				{
					$('.toc i').addClass('on').next().slideDown();
				}
			}
			else
			{
				$(this).toggleClass('on').next().slideToggle();
			}
		});
	}

	if ($('.toc-full').length > 0)
	{
		$('.main-body').scrollNav({
			sections:		'.part-link',
			subSections:	'.toc-full>li',
			sectionElem:	'div',
			insertTarget:	$('.left-column').get(0),
			insertLocation:	'prependTo',
			fixedMargin:	1000,
			showHeadline:	false,
			scrollToHash:	false,
			showTopLink:	false
		});
	}

	if ($sn)
	{
		snTop = $sn.offset().top;
		snInHeight = $sn.outerHeight();

		isScrolled = true;
		checkScroll();
	}
});

$(window).load(function(){
	if ($sn)
	{
		$sn.removeClass('true-fixed');
		snTop = $sn.offset().top;
	}
});

$(window).scroll(function(){
	isScrolled = true;
	wD = $(window).scrollTop() > wS;
	wS = $(window).scrollTop();
});

$sn.scroll(function(){
	if ($sn && $sn.hasClass('true-fixed'))
	{
		snScroll = $(this).scrollTop();
	}
});

$(window).resize(function(){
	ww = $(window).width();
	wh = $(window).height();
	if ($sn)
	{
		isScrolled = false;
		$sn.removeClass('true-fixed').addClass('top').removeClass('bottom');
		snTop = $sn.offset().top;
		snInHeight = $sn.outerHeight();
		if ($sn.hasClass('bottom') && false)
		{
			snScroll = snInHeight - $(window).height();
		}
	}
	isScrolled = true;
});
