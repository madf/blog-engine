let blocks = [];
let blockIdCounter = 0;
let contentElement = document.getElementById('content');

function addTextBlock(text) {
  const block = {
    id: ++blockIdCounter,
    type: 'text',
    content: text
  };
  blocks.push(block);
}

function addCarouselBlock() {
    const block = {
        id: ++blockIdCounter,
        type: 'carousel',
        images: []
    };
    blocks.push(block);
}

function deleteBlock(blockId) {
    blocks = blocks.filter(block => block.id !== blockId);
    renderBlocks();
}

function moveBlockUp(blockId) {
    const index = blocks.findIndex(block => block.id === blockId);
    if (index > 0) {
        [blocks[index], blocks[index - 1]] = [blocks[index - 1], blocks[index]];
        renderBlocks();
    }
}

function moveBlockDown(blockId) {
    const index = blocks.findIndex(block => block.id === blockId);
    if (index < blocks.length - 1) {
        [blocks[index], blocks[index + 1]] = [blocks[index + 1], blocks[index]];
        renderBlocks();
    }
}

function updateTextContent(blockId, content) {
    const block = blocks.find(b => b.id === blockId);
    if (block) {
        block.content = content;
    }
}

const handleImageUpload = (blockId, files) => {
  const block = blocks.find(b => b.id === blockId);
  if (!block || block.type !== 'carousel') return;

  Array.from(files).forEach(file => {
    if (file.type.startsWith('image/')) {
      const reader = new FileReader();
      reader.onload = (e) => {
        const imageId = Date.now() + Math.random();
        block.images.push({
            id: imageId,
            url: e.target.result, // In real app, this would be from upload API
            caption: '',
            filename: file.name
        });
        renderBlocks();
      };
      reader.readAsDataURL(file);
    }
  });
}

function updateImageCaption(blockId, imageId, caption) {
    const block = blocks.find(b => b.id === blockId);
    if (block) {
        const image = block.images.find(img => img.id === imageId);
        if (image) {
            image.caption = caption;
        }
    }
}

function deleteImage(blockId, imageId) {
    const block = blocks.find(b => b.id === blockId);
    if (block) {
        block.images = block.images.filter(img => img.id !== imageId);
        renderBlocks();
    }
}

const createButton = (className, handler, name) => {
  const btn = document.createElement('button');
  btn.className = className;
  btn.innerHTML = name;
  btn.addEventListener('click', handler);
  return btn;
};

const createBlockHeader = (block, index, title) => {
  const ctrl = document.createElement('div');
  ctrl.className = 'block-controls';
  const upBtn = createButton('btn btn-small btn-secondary', () => { moveBlockUp(block.id); }, '↑');
  upBtn.disabled = index === 0;
  const downBtn = createButton('btn btn-small btn-secondary', () => { moveBlockDown(block.id); }, '↓');
  downBtn.disabled = index === blocks.length - 1;
  const deleteBtn = createButton('btn btn-small btn-danger', () => { deleteBlock(block.id); }, 'Delete');
  ctrl.appendChild(upBtn);
  ctrl.appendChild(downBtn);
  ctrl.appendChild(deleteBtn);
  const hdr = document.createElement('div');
  hdr.className = 'block-header';
  hdr.innerHTML = `<span class="block-type">${title}</span>`;
  hdr.appendChild(ctrl);
  return hdr;
};

const createTextContent = block => {
  const ta = document.createElement('textarea');
  ta.placeholder = 'Enter your text here...';
  ta.addEventListener('input', e => { updateTextContent(block.id, e.currentTarget.value); });
  ta.innerHTML = block.content;
  const tb = document.createElement('div');
  tb.className = 'text-block';
  tb.appendChild(ta);
  const cnt = document.createElement('div');
  cnt.className = 'block-content';
  cnt.appendChild(tb);
  return cnt;
};

const createImageCaption = (block, img) => {
  const ic = document.createElement('div');
  ic.className = 'image-caption';
  const ici = document.createElement('input');
  ici.type = 'text';
  ici.placeholder = 'Image caption...';
  ici.value = img.caption;
  ici.addEventListener('input', e => { updateImageCaption(block.id, img.id, e.currentTarget.value); });
  ic.appendChild(ici);
  return ic;
};

const createImageControls = (block, img) => {
  /*
    <div class="image-controls">
      <small>${image.filename}</small>
      <button class="btn btn-small btn-danger" onclick="deleteImage(${block.id}, ${image.id})">×</button>
    </div>
   */
  const ic = document.createElement('div');
  ic.className = 'image-controls';
  const fn = document.createElement('small');
  fn.innerHTML = img.filename;
  ic.appendChild(fn);
  ic.appendChild(createButton('btn btn-small btn-danger', () => { deleteImage(block.id, img.id); }, 'x'));
  return ic;
};

const createImageUpload = block => {
  /*
    <div class="image-upload">
      <label for="upload-${block.id}" class="upload-btn">
        📁 Upload Images
      </label>
      <input
        type="file"
        id="upload-${block.id}"
        multiple
        accept="image/*"
        onchange="handleImageUpload(${block.id}, this.files)"
      >
    </div>
   */
  const iu = document.createElement('div');
  iu.className = 'image-upload';
  iu.innerHTML = `<label for="upload-${block.id}" class="upload-btn">📁 Upload Images</label>`;
  const iui = document.createElement('input');
  iui.type = 'file';
  iui.id = `upload-${block.id}`;
  iui.multiple = true;
  iui.accept="image/*";
  iui.addEventListener('change', e => { handleImageUpload(block.id, e.currentTarget.files); });
  iu.appendChild(iui);
  return iu;
};

const createImageItem = (block, img) => {
  const ii = document.createElement('div');
  ii.className = 'image-item';
  ii.innerHTML = `<img src="${img.url}" alt="${img.caption}" class="image-preview">`;
  ii.appendChild(createImageCaption(block, img));
  ii.appendChild(createImageControls(block, img));
  return ii;
};

const createCarouselContent = block => {
  const cb = document.createElement('div');
  cb.className = 'carousel-block';
  cb.appendChild(createImageUpload(block));
  console.log(`Carousel images: ${block.images.length}`);
  if (block.images.length > 0)
  {
    const ig = document.createElement('div');
    ig.className = 'images-grid';
    block.images.map(img => {
      ig.appendChild(createImageItem(block, img));
    });
    cb.appendChild(ig);
  }
  const cnt = document.createElement('div');
  cnt.className = 'block-content';
  cnt.appendChild(cb);
  return cnt;
};

const createTextBlock = (block, index) => {
  const blk = document.createElement('div');
  blk.className = 'block';
  blk.appendChild(createBlockHeader(block, index, `Text Block ${index + 1}`));
  blk.appendChild(createTextContent(block));
  return blk;
};

const createCarouselBlock = (block, index) => {
  const blk = document.createElement('div');
  blk.className = 'block';
  blk.appendChild(createBlockHeader(block, index, `Carousel Block ${index + 1}`));
  blk.appendChild(createCarouselContent(block));
  return blk;
};

function renderBlocks() {
  const container = document.getElementById('blocksContainer');

  if (blocks.length === 0) {
    container.innerHTML = `
      <div class="empty-state">
        <p>No blocks yet. Add a text block or carousel to get started.</p>
      </div>
    `;
    return;
  }

  container.textContent = '';

  blocks.map((block, index) => {
    if (block.type === 'text') {
      container.appendChild(createTextBlock(block, index));
    } else if (block.type === 'carousel') {
      container.appendChild(createCarouselBlock(block, index));
    }
  });

  /*
  container.innerHTML = blocks.map((block, index) => {
    if (block.type === 'text') {
      return `
        <div class="block">
          <div class="block-header">
            <span class="block-type">Text Block #${index + 1}</span>
            <div class="block-controls">
              <button class="btn btn-small btn-secondary" onclick="moveBlockUp(${block.id})" ${index === 0 ? 'disabled' : ''}>↑</button>
              <button class="btn btn-small btn-secondary" onclick="moveBlockDown(${block.id})" ${index === blocks.length - 1 ? 'disabled' : ''}>↓</button>
              <button class="btn btn-small btn-danger" onclick="deleteBlock(${block.id})">Delete</button>
            </div>
          </div>
          <div class="block-content">
            <div class="text-block">
              <textarea
                placeholder="Enter your text here..."
                oninput="updateTextContent(${block.id}, this.value)"
              >${block.content}</textarea>
            </div>
          </div>
        </div>
      `;
    } else if (block.type === 'carousel') {
      return `
        <div class="block">
          <div class="block-header">
            <span class="block-type">Carousel Block #${index + 1}</span>
            <div class="block-controls">
              <button class="btn btn-small btn-secondary" onclick="moveBlockUp(${block.id})" ${index === 0 ? 'disabled' : ''}>↑</button>
              <button class="btn btn-small btn-secondary" onclick="moveBlockDown(${block.id})" ${index === blocks.length - 1 ? 'disabled' : ''}>↓</button>
              <button class="btn btn-small btn-danger" onclick="deleteBlock(${block.id})">Delete</button>
            </div>
          </div>
          <div class="block-content">
            <div class="carousel-block">
              <div class="image-upload">
                <label for="upload-${block.id}" class="upload-btn">
                  📁 Upload Images
                </label>
                <input
                  type="file"
                  id="upload-${block.id}"
                  multiple
                  accept="image/*"
                  onchange="handleImageUpload(${block.id}, this.files)"
                >
              </div>
              ${block.images.length > 0 ? `
                <div class="images-grid">
                  ${block.images.map(image => `
                    <div class="image-item">
                      <img src="${image.url}" alt="${image.caption}" class="image-preview">
                      <div class="image-caption">
                        <input
                          type="text"
                          placeholder="Image caption..."
                          value="${image.caption}"
                          oninput="updateImageCaption(${block.id}, ${image.id}, this.value)"
                        >
                      </div>
                      <div class="image-controls">
                        <small>${image.filename}</small>
                        <button class="btn btn-small btn-danger" onclick="deleteImage(${block.id}, ${image.id})">×</button>
                      </div>
                    </div>
                  `).join('')}
                </div>
              ` : ''}
            </div>
          </div>
        </div>
      `;
    }
  }).join('');*/
}

function saveDraft() {
  const postData = {
    title: document.getElementById('postTitle').value,
    blocks: blocks,
    status: 'draft'
  };

  console.log('Saving draft:', postData);
  document.getElementById('saveStatus').textContent = 'Draft saved at ' + new Date().toLocaleTimeString();

  // In real app: fetch('/api/posts', { method: 'POST', body: JSON.stringify(postData) })
}

function publishPost() {
  const postData = {
    title: document.getElementById('postTitle').value,
    blocks: blocks,
    status: 'published'
  };

  console.log('Publishing post:', postData);
  document.getElementById('saveStatus').textContent = 'Published at ' + new Date().toLocaleTimeString();

  // In real app: fetch('/api/posts/publish', { method: 'POST', body: JSON.stringify(postData) })
}

const savePost = () => {
  const contentElement = document.getElementById('content');
  contentElement.value = JSON.stringify(blocks);
  const form = document.getElementById('post-form');
  form.submit();
};

// Initialize with some sample data
addTextBlock('This is a sample text block. You can edit this content and add more blocks below.');
addCarouselBlock();
renderBlocks();

const atbb = document.getElementById('add-text-block-button');
atbb.addEventListener('click', e => { addTextBlock(''); renderBlocks(); });
const acbb = document.getElementById('add-carousel-block-button');
acbb.addEventListener('click', e => { addCarouselBlock(); renderBlocks(); });
const sb = document.getElementById('save-button');
sb.addEventListener('click', e => { savePost(); });
