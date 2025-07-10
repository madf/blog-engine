let blockIdCounter = 0;
const contentElement = document.getElementById('content');
let post = undefined;

const restoreOrCreatePost = async () => {
  const post = window.sessionStorage.getItem('post');
  if (post) {
    return JSON.parse(post);
  }

  try {
    const formData = new FormData();
    formData.append('title', '');
    formData.append('content', '[]');
    resp = await fetch('/admin/api/post', {
      method: 'POST',
      body: formData
    });
    if (!resp.ok) {
      throw new Error(`Failed to create a new post: ${resp.statusText}`);
    }
    const post = await resp.json();
    window.sessionStorage.setItem('post', JSON.stringify(post));
    return post;
  } catch (error) {
    console.log(error);
  }
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

const deleteBlock = blockId => {
  post.content = post.content.filter(block => block.id !== blockId);
  renderBlocks();
}

const moveBlockUp = blockId => {
  const index = post.content.findIndex(block => block.id === blockId);
  if (index > 0) {
    [post.content[index], post.content[index - 1]] = [post.content[index - 1], post.content[index]];
    renderBlocks();
  }
}

const moveBlockDown = blockId => {
  const index = post.content.findIndex(block => block.id === blockId);
  if (index < post.content.length - 1) {
    [post.content[index], post.content[index + 1]] = [post.content[index + 1], post.content[index]];
    renderBlocks();
  }
}

const updateTextContent = (blockId, content) => {
  console.log(`New text for block ${blockId}: ${content}`);
  const block = post.content.find(b => b.id === blockId);
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

const handleImageUpload = async (blockId, files) => {
  const block = post.content.find(b => b.id === blockId);
  if (!block || block.type !== 'carousel') return;
  let needSave = false;

  for (const file of files) {
    if (!file.type.startsWith('image/')) {
      continue;
    }

    // Show loading state
    const tempId = Date.now() + Math.random();
    block.content.push({
      id: tempId,
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
      const imageIndex = block.content.findIndex(img => img.id === tempId);
      if (imageIndex !== -1) {
        block.content[imageIndex] = result[0];
        renderBlocks();
        needSave = true;
      }
    } catch (error) {
      console.error('Upload failed:', error);
      // Remove failed upload from UI
      block.content = block.content.filter(img => img.id !== tempId);
      alert(`Failed to upload ${file.name}: ${error.message}`);
      renderBlocks();
    }
  }

  if (needSave) {
    savePost();
  }
}

function updateImageCaption(blockId, imageId, caption) {
    const block = post.content.find(b => b.id === blockId);
    if (block) {
        const image = block.content.find(img => img.id === imageId);
        if (image) {
            image.caption = caption;
        }
    }
}

const deleteImage = async (blockId, imageId) => {
  const block = post.content.find(b => b.id === blockId);
  if (block) {
    try {
      resp = await fetch(`/admin/api/image/${imageId}`, {
        method: 'DELETE',
      });
      if (!resp.ok) {
        throw new Error(`Failed to delete image: ${resp.statusText}`);
      }
      block.content = block.content.filter(img => img.id !== imageId);
    } catch (error) {
      console.log(error);
    }
    savePost();
    renderBlocks();
  }
};

const moveImageLeft = (blockId, imageId) => {
  const block = post.content.find(b => b.id === blockId);
  if (block) {
    const idx = block.content.findIndex(img => img.id == imageId);
    if (idx > 0) {
      [block.content[idx - 1], block.content[idx]] = [block.content[idx], block.content[idx - 1]];
      renderBlocks();
    }
  }
};

const moveImageRight = (blockId, imageId) => {
  const block = post.content.find(b => b.id === blockId);
  if (block) {
    const idx = block.content.findIndex(img => img.id == imageId);
    if (idx < block.content.length - 1) {
      [block.content[idx], block.content[idx + 1]] = [block.content[idx + 1], block.content[idx]];
      renderBlocks();
    }
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

const createBlockHeader = (block, index, title) => {
  const ctrl = document.createElement('div');
  ctrl.className = 'block-controls';
  const upBtn = createButton('btn btn-small btn-secondary', () => { moveBlockUp(block.id); }, '↑');
  upBtn.disabled = index === 0;
  const downBtn = createButton('btn btn-small btn-secondary', () => { moveBlockDown(block.id); }, '↓');
  downBtn.disabled = index === post.content.length - 1;
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
  fn.innerHTML = img.file_name;
  fn.title = img.file_name;
  ic.appendChild(fn);
  ic.appendChild(createButton('btn btn-small btn-secondary', () => { moveImageLeft(block.id, img.id); }, '<'));
  ic.appendChild(createButton('btn btn-small btn-secondary', () => { moveImageRight(block.id, img.id); }, '>'));
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
  ii.innerHTML = `<img src="${img.preview_url}" alt="${img.caption}" class="image-preview">`;
  ii.appendChild(createImageCaption(block, img));
  ii.appendChild(createImageControls(block, img));
  return ii;
};

const createCarouselContent = block => {
  const cb = document.createElement('div');
  cb.className = 'carousel-block';
  cb.appendChild(createImageUpload(block));
  console.log(`Carousel images: ${block.content.length}`);
  if (block.content.length > 0)
  {
    const ig = document.createElement('div');
    ig.className = 'images-grid';
    block.content.map(img => {
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

  if (post.content.length === 0) {
    container.innerHTML = `
      <div class="empty-state">
        <p>No blocks yet. Add a text block or carousel to get started.</p>
      </div>
    `;
    return;
  }

  container.textContent = '';

  post.content.map((block, index) => {
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

const onLoad = async () => {
  post = await restoreOrCreatePost();
  renderBlocks();
};

const atbb = document.getElementById('add-text-block-button');
atbb.addEventListener('click', e => { addTextBlock(); renderBlocks(); });
const acbb = document.getElementById('add-carousel-block-button');
acbb.addEventListener('click', e => { addCarouselBlock(); renderBlocks(); });
const sb = document.getElementById('save-button');
sb.addEventListener('click', e => { savePost(); });
window.addEventListener('load', e => { onLoad(); });
