# CL-KL-WGPU: Common Lisp Bindings for `wgpu-native`

A very **experimental**, **dirty**, and outright **desperate** attempt to teach my poor, overworked brain some Common Lisp by forcing it to write bindings for the [wgpu-native](https://github.com/gfx-rs/wgpu-native) library.

Couldn't have picked a more spectacularly unsuitable subject for a "study project," but it is what it is! 🤪

That said, I hope I can make this work, and so far, it's actually going surprisingly well (relatively of course, we are talking about CL after all). I can grab an adapter and a device on X11, which is, like, half the battle for reproducing the wgpu-native [triangle example](https://github.com/gfx-rs/wgpu-native/blob/trunk/examples/triangle/main.c). The other half - shaders and pipelines are my next stop.

---

## 🤷 Getting Started (Don't Bother Yet)

In case you are still reading after the brutal (and highly accurate) intro I gave, I want to know what happened to you!?

Currently, the other part of the project - my custom GLFW3 bindings is not published yet. 💃

But if you are on **Arch Linux**, you can at least get the underlying library installed while you patiently wait for me to stop banging my head against the keyboard.

1.  **Install `wgpu-native`:**
    You can grab the library from the AUR:
    * [wgpu-native-git](https://aur.archlinux.org/packages/wgpu-native-git)

### **🔥 Stay Tuned!** (or run away) 🔥
