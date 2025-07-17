const getBlockImages = bid => {
  const b = document.getElementById(bid);
  if (!b) {
    console.log('Unknown block id: ' + bid);
    return [];
  }
  return [...b.querySelectorAll('.carousel-fade')];
};

const getDots = bid => {
  const b = document.getElementById(bid);
  if (!b) {
    console.log('Unknown block id: ' + bid);
    return [];
  }
  return [...b.querySelectorAll('.carousel-dot')];
};

const findBlockImage = is => {
  return is.findIndex(i => i.checkVisibility());
};

const carouselPrevImage = bid => {
  const is = getBlockImages(bid);
  let idx = findBlockImage(is);
  if (idx < 0) {
    console.log('No block images');
    return;
  }
  if (idx > 0) {
    is[idx].style.display = 'none';
    is[idx - 1].style.display = 'block';
    const dots = getDots(bid);
    for (i = 0; i < dots.length; ++i) {
      if (i == idx - 1) {
        dots[i].classList.add("carousel-dot-active");
      } else {
        dots[i].classList.remove("carousel-dot-active");
      }
    }
  }
};

const carouselNextImage = bid => {
  const is = getBlockImages(bid);
  let idx = findBlockImage(is);
  if (idx < 0) {
    console.log('No block images');
    return;
  }
  if (idx < is.length - 1) {
    is[idx].style.display = 'none';
    is[idx + 1].style.display = 'block';
    const dots = getDots(bid);
    for (i = 0; i < dots.length; ++i) {
      if (i == idx + 1) {
        dots[i].classList.add("carousel-dot-active");
      } else {
        dots[i].classList.remove("carousel-dot-active");
      }
    }
  }
};

const carouselShowImage = (bid, idx) => {
  const is = getBlockImages(bid);
  const dots = getDots(bid);
  for (i = 0; i < is.length; ++i) {
    if (i == idx) {
      is[i].style.display = 'block';
      dots[i].classList.add("carousel-dot-active");
    } else {
      is[i].style.display = 'none';
      dots[i].classList.remove("carousel-dot-active");
    }
  }
};

const onLoad = () => {
  const bs = [...document.getElementsByClassName('carousel-preview-block')];
  bs.forEach(b => {
    const prev = b.querySelector('.carousel-btn-prev');
    prev.addEventListener('click', e => { carouselPrevImage(b.id); });
    const next = b.querySelector('.carousel-btn-next');
    next.addEventListener('click', e => { carouselNextImage(b.id); });
    const dots = getDots(b.id);
    dots.forEach((d, idx) => {
      d.addEventListener('click', e => { carouselShowImage(b.id, idx); });
    });
  });
};

window.addEventListener('load', e => { onLoad(); });
