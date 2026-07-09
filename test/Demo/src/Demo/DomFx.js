export function scrollIntoViewImpl(id) {
  return function () {
    var el = document.getElementById(id);
    if (el && el.scrollIntoView) {
      // Instant, not "smooth" - during a fast-moving run this fires on
      // every single step, and a queued/animated scroll would visibly lag
      // behind (or jitter, restarting mid-animation on each new step)
      // instead of just tracking the latest square directly.
      el.scrollIntoView({ inline: "nearest", block: "nearest", behavior: "auto" });
    }
  };
}

export function mouseOffsetImpl(ev) {
  return function () {
    return { x: ev.offsetX, y: ev.offsetY };
  };
}
