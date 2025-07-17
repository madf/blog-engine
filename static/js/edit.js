let blockIdCounter = 0;
let post = undefined;

const extractPost = () => {
  return JSON.parse(document.getElementById('post').value);
};

const addTextBlock = () => {
  const block = {
    id: ++blockIdCounter,
    type: 'text',
    content: ''
  };
  post.content.push(block);
}

const addCarouselBlock = () => {
  const block = {
    id: ++blockIdCounter,
    type: 'carousel',
    content: []
  };
  post.content.push(block);
}

const deleteBlock = idx => {
  post.content.splice(idx, 1);
  renderBlocks();
}

const moveBlockUp = idx => {
  if (idx > 0) {
    [post.content[idx], post.content[idx - 1]] = [post.content[idx - 1], post.content[idx]];
    renderBlocks();
  }
}

const moveBlockDown = idx => {
  if (idx < post.content.length - 1) {
    [post.content[idx], post.content[idx + 1]] = [post.content[idx + 1], post.content[idx]];
    renderBlocks();
  }
}

const updateTextContent = (idx, content) => {
  console.log(`New text for block ${idx}: ${content}`);
  const block = post.content[idx];
  if (block) {
    block.content = content;
  }
}

/*
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
*/

const handleImageUpload = async (blockIdx, files) => {
  const block = post.content[blockIdx];
  if (!block || block.type !== 'carousel') return;
  let needSave = false;

  for (const file of files) {
    if (!file.type.startsWith('image/')) {
      continue;
    }

    // Show loading state
    block.content.push({
      preview_url: '/images/loading-placeholder.svg', // Loading placeholder
      caption: '',
      file_name: file.name,
      uploading: true
    });
    renderBlocks();

    try {
      // Upload immediately
      const formData = new FormData();
      formData.append('image', file);

      const response = await fetch(`/admin/api/post/${post.id}/image`, {
        method: 'POST',
        body: formData,
        credentials: 'include' // Include session cookie
      });

      if (!response.ok) {
        throw new Error(`Upload failed: ${response.statusText}`);
      }

      const result = await response.json();
      if (result.length !== 1) {
        throw new Error('Image upload returned multiple results');
      }

      // Replace temp image with uploaded result
      const imageIndex = block.content.findIndex(img => img.file_name === file.name && img.uploading);
      if (imageIndex !== -1) {
        block.content[imageIndex] = result[0];
        renderBlocks();
        needSave = true;
      }
    } catch (error) {
      console.error('Upload failed:', error);
      // Remove failed upload from UI
      block.content = block.content.filter(img => img.file_name !== file.name || img.uploading);
      alert(`Failed to upload ${file.name}: ${error.message}`);
      renderBlocks();
    }
  }

  if (needSave) {
    savePost();
  }
}

const updateImageCaption = (blockIdx, idx, caption) => {
  const block = post.content[blockIdx];
  if (block) {
    const image = block.content[idx];
    if (image) {
      image.caption = caption;
    }
  }
};

const saveImageCaption = async (blockIdx, idx) => {
  const block = post.content[blockIdx];
  if (block) {
    const image = block.content[idx];
    if (image) {
      try {
        const formData = new FormData();
        formData.append('caption', image.caption);

        const response = await fetch(`/admin/api/image/${image.id}`, {
          method: 'PUT',
          body: formData,
          credentials: 'include' // Include session cookie
        });

        if (!response.ok) {
          throw new Error(`Upload failed: ${response.statusText}`);
        }
      } catch (error) {
        console.error('Image caption update failed:', error);
      }
    }
  }
};

const deleteImage = async (blockIdx, idx) => {
  const block = post.content[blockIdx];
  if (!block) {
    console.log(`Unknown block idx: {blockIdx}`);
    return;
  }
  const img = block.content[idx];
  if (block && img) {
    try {
      resp = await fetch(`/admin/api/image/${img.id}`, {
        method: 'DELETE',
      });
      if (!resp.ok) {
        throw new Error(`Failed to delete image: ${resp.statusText}`);
      }
      block.content.splice(idx, 1);
    } catch (error) {
      console.log(error);
    }
    savePost();
    renderBlocks();
  }
};

const moveImageLeft = (blockIdx, idx) => {
  const block = post.content[blockIdx];
  if (block && idx > 0) {
    [block.content[idx - 1], block.content[idx]] = [block.content[idx], block.content[idx - 1]];
    renderBlocks();
  }
};

const moveImageRight = (blockIdx, idx) => {
  const block = post.content[blockIdx];
  if (block && idx < block.content.length - 1) {
    [block.content[idx], block.content[idx + 1]] = [block.content[idx + 1], block.content[idx]];
    renderBlocks();
  }
};

const createButton = (className, handler, name) => {
  const btn = document.createElement('button');
  btn.className = className;
  btn.innerHTML = name;
  btn.type = 'button';
  btn.addEventListener('click', handler);
  return btn;
};

const createBlockHeader = (idx, title) => {
  const ctrl = document.createElement('div');
  ctrl.className = 'block-controls';
  const upBtn = createButton('btn btn-small btn-secondary', () => { moveBlockUp(idx); }, '↑');
  upBtn.disabled = idx === 0;
  const downBtn = createButton('btn btn-small btn-secondary', () => { moveBlockDown(idx); }, '↓');
  downBtn.disabled = idx === post.content.length - 1;
  const deleteBtn = createButton('btn btn-small btn-danger', () => { deleteBlock(idx); }, 'Delete');
  ctrl.appendChild(upBtn);
  ctrl.appendChild(downBtn);
  ctrl.appendChild(deleteBtn);
  const hdr = document.createElement('div');
  hdr.className = 'block-header';
  hdr.innerHTML = `<span class="block-type">${title}</span>`;
  hdr.appendChild(ctrl);
  return hdr;
};

const createTextContent = (block, idx) => {
  const ta = document.createElement('textarea');
  ta.placeholder = 'Enter your text here...';
  ta.addEventListener('input', e => { updateTextContent(idx, e.currentTarget.value); });
  ta.innerHTML = block.content;
  const tb = document.createElement('div');
  tb.className = 'text-block';
  tb.appendChild(ta);
  const cnt = document.createElement('div');
  cnt.className = 'block-content';
  cnt.appendChild(tb);
  return cnt;
};

const createImageCaption = (blockIdx, img, idx) => {
  const ic = document.createElement('div');
  ic.className = 'image-caption';
  const ici = document.createElement('input');
  ici.type = 'text';
  ici.placeholder = 'Image caption...';
  ici.value = img.caption;
  ici.addEventListener('input', e => { updateImageCaption(blockIdx, idx, e.currentTarget.value); });
  ic.appendChild(ici);
  const icb = createButton('btn btn-small btn-secondary', () => { saveImageCaption(blockIdx, idx); }, '💾')
  ic.appendChild(icb);
  return ic;
};

const createImageControls = (blockIdx, img, idx) => {
  /*
    <div class="image-controls">
      <small>${image.filename}</small>
      <button class="btn btn-small btn-danger" onclick="deleteImage(${block.id}, ${image.id})">×</button>
    </div>
   */
  const ic = document.createElement('div');
  ic.className = 'image-controls';
  const fn = document.createElement('small');
  fn.innerHTML = img.file_name;
  fn.title = img.file_name;
  ic.appendChild(fn);
  ic.appendChild(createButton('btn btn-small btn-secondary', () => { moveImageLeft(blockIdx, idx); }, '<'));
  ic.appendChild(createButton('btn btn-small btn-secondary', () => { moveImageRight(blockIdx, idx); }, '>'));
  ic.appendChild(createButton('btn btn-small btn-danger', () => { deleteImage(blockIdx, idx); }, 'x'));
  return ic;
};

const createImageUpload = blockIdx => {
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
  iu.innerHTML = `<label for="upload-${blockIdx}" class="upload-btn">📁 Upload Images</label>`;
  const iui = document.createElement('input');
  iui.type = 'file';
  iui.id = `upload-${blockIdx}`;
  iui.multiple = true;
  iui.accept="image/*";
  iui.addEventListener('change', e => { handleImageUpload(blockIdx, e.currentTarget.files); });
  iu.appendChild(iui);
  return iu;
};

const createImageItem = (block, blockIdx, img, idx) => {
  const ii = document.createElement('div');
  ii.className = 'image-item';
  ii.innerHTML = `<img src="${img.preview_url}" alt="${img.caption}" class="image-preview">`;
  ii.appendChild(createImageCaption(blockIdx, img, idx));
  ii.appendChild(createImageControls(blockIdx, img, idx));
  return ii;
};

const createCarouselContent = (block, blockIdx) => {
  const cb = document.createElement('div');
  cb.className = 'carousel-block';
  cb.appendChild(createImageUpload(blockIdx));
  console.log(`Carousel images: ${block.content.length}`);
  if (block.content.length > 0)
  {
    const ig = document.createElement('div');
    ig.className = 'images-grid';
    block.content.map((img, idx) => {
      ig.appendChild(createImageItem(block, blockIdx, img, idx));
    });
    cb.appendChild(ig);
  }
  const cnt = document.createElement('div');
  cnt.className = 'block-content';
  cnt.appendChild(cb);
  return cnt;
};

const createTextBlock = (block, idx) => {
  const blk = document.createElement('div');
  blk.className = 'block';
  blk.appendChild(createBlockHeader(idx, `Text Block ${idx + 1}`));
  blk.appendChild(createTextContent(block, idx));
  return blk;
};

const createCarouselBlock = (block, idx) => {
  const blk = document.createElement('div');
  blk.className = 'block';
  blk.appendChild(createBlockHeader(idx, `Carousel Block ${idx + 1}`));
  blk.appendChild(createCarouselContent(block, idx));
  return blk;
};

function renderBlocks() {
  const container = document.getElementById('blocksContainer');

  if (post.content.length === 0) {
    container.innerHTML = `
      <div class="empty-state">
        <p>No blocks yet. Add a text block or carousel to get started.</p>
      </div>
    `;
    return;
  }

  container.textContent = '';

  post.content.map((block, idx) => {
    if (block.type === 'text') {
      container.appendChild(createTextBlock(block, idx));
    } else if (block.type === 'carousel') {
      container.appendChild(createCarouselBlock(block, idx));
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
    blocks: post.content,
    status: 'draft'
  };

  console.log('Saving draft:', postData);
  document.getElementById('saveStatus').textContent = 'Draft saved at ' + new Date().toLocaleTimeString();

  // In real app: fetch('/api/posts', { method: 'POST', body: JSON.stringify(postData) })
}

function publishPost() {
  const postData = {
    title: document.getElementById('postTitle').value,
    blocks: post.content,
    status: 'published'
  };

  console.log('Publishing post:', postData);
  document.getElementById('saveStatus').textContent = 'Published at ' + new Date().toLocaleTimeString();

  // In real app: fetch('/api/posts/publish', { method: 'POST', body: JSON.stringify(postData) })
}

const savePost = async () => {
  try {
    // Upload immediately
    const formData = new FormData();
    formData.append('title', document.getElementById('title').value);
    formData.append('content', JSON.stringify(post.content));
    formData.append('draft', document.getElementById('is_draft').checked);

    const response = await fetch(`/admin/api/post/${post.id}`, {
      method: 'PUT',
      body: formData,
      credentials: 'include' // Include session cookie
    });

    if (!response.ok) {
      throw new Error(`Upload failed: ${response.statusText}`);
    }

    window.sessionStorage.setItem('post', JSON.stringify(post));
  } catch (error) {
    console.log(error);
  }
};

const onLoad = () => {
  post = extractPost();
  renderBlocks();
};

const atbb = document.getElementById('add-text-block-button');
atbb.addEventListener('click', e => { addTextBlock(); renderBlocks(); });
const acbb = document.getElementById('add-carousel-block-button');
acbb.addEventListener('click', e => { addCarouselBlock(); renderBlocks(); });
const sb = document.getElementById('save-button');
sb.addEventListener('click', e => { savePost(); });
window.addEventListener('load', e => { onLoad(); });
