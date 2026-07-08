export function naturalWidth(img) {
  return img.naturalWidth;
}

export function naturalHeight(img) {
  return img.naturalHeight;
}

export function imageDataToArray(imageData) {
  return Array.from(imageData.data);
}
