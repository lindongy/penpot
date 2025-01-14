mod debug;
mod math;
pub mod mem;
mod render;
mod shapes;
mod state;
mod utils;
mod view;

use skia_safe as skia;

use crate::shapes::Image;
use crate::state::State;
use crate::utils::uuid_from_u32_quartet;

static mut STATE: Option<Box<State>> = None;

extern "C" {
    fn emscripten_GetProcAddress(
        name: *const ::std::os::raw::c_char,
    ) -> *const ::std::os::raw::c_void;
}

fn init_gl() {
    unsafe {
        gl::load_with(|addr| {
            let addr = std::ffi::CString::new(addr).unwrap();
            emscripten_GetProcAddress(addr.into_raw() as *const _) as *const _
        });
    }
}

/// This is called from JS after the WebGL context has been created.
#[no_mangle]
pub extern "C" fn init(width: i32, height: i32) {
    let state_box = Box::new(State::new(width, height, 2048));
    unsafe {
        STATE = Some(state_box);
    }
}

#[no_mangle]
pub extern "C" fn set_render_options(debug: u32, dpr: f32) {
    let state = unsafe { STATE.as_mut() }.expect("got an invalid state pointer");
    let render_state = state.render_state();

    render_state.set_debug_flags(debug);
    render_state.set_dpr(dpr);
}

#[no_mangle]
pub unsafe extern "C" fn render() {
    let state = unsafe { STATE.as_mut() }.expect("got an invalid state pointer");
    state.render_all(true);
}

#[no_mangle]
pub unsafe extern "C" fn render_without_cache() {
    let state = unsafe { STATE.as_mut() }.expect("got an invalid state pointer");
    state.render_all(false);
}

#[no_mangle]
pub unsafe extern "C" fn navigate() {
    let state = unsafe { STATE.as_mut() }.expect("got an invalid state pointer");
    state.navigate();
}

#[no_mangle]
pub extern "C" fn reset_canvas() {
    let state = unsafe { STATE.as_mut() }.expect("got an invalid state pointer");
    state.render_state().reset_canvas();
}

#[no_mangle]
pub extern "C" fn resize_viewbox(width: i32, height: i32) {
    let state = unsafe { STATE.as_mut() }.expect("got an invalid state pointer");
    state.resize(width, height);
}

#[no_mangle]
pub extern "C" fn set_view(zoom: f32, x: f32, y: f32) {
    let state = unsafe { STATE.as_mut() }.expect("got an invalid state pointer");
    state.render_state().viewbox.set_all(zoom, x, y);
}

#[no_mangle]
pub extern "C" fn set_view_zoom(zoom: f32) {
    let state = unsafe { STATE.as_mut() }.expect("got an invalid state pointer");
    state.render_state().viewbox.set_zoom(zoom);
}

#[no_mangle]
pub extern "C" fn set_view_xy(x: f32, y: f32) {
    let state = unsafe { STATE.as_mut() }.expect("got an invalid state pointer");
    state.render_state().viewbox.set_pan_xy(x, y);
}

#[no_mangle]
pub extern "C" fn use_shape(a: u32, b: u32, c: u32, d: u32) {
    let state = unsafe { STATE.as_mut() }.expect("got an invalid state pointer");
    let id = uuid_from_u32_quartet(a, b, c, d);
    state.use_shape(id);
}

#[no_mangle]
pub unsafe extern "C" fn set_shape_selrect(left: f32, top: f32, right: f32, bottom: f32) {
    let state = unsafe { STATE.as_mut() }.expect("got an invalid state pointer");

    if let Some(shape) = state.current_shape() {
        shape.selrect.set_ltrb(left, top, right, bottom);
    }
}

#[no_mangle]
pub unsafe extern "C" fn set_shape_rotation(rotation: f32) {
    let state = unsafe { STATE.as_mut() }.expect("got an invalid state pointer");
    if let Some(shape) = state.current_shape() {
        shape.rotation = rotation;
    }
}

#[no_mangle]
pub unsafe extern "C" fn set_shape_transform(a: f32, b: f32, c: f32, d: f32, e: f32, f: f32) {
    let state = unsafe { STATE.as_mut() }.expect("got an invalid state pointer");
    if let Some(shape) = state.current_shape() {
        shape.transform.a = a;
        shape.transform.b = b;
        shape.transform.c = c;
        shape.transform.d = d;
        shape.transform.e = e;
        shape.transform.f = f;
    }
}

#[no_mangle]
pub extern "C" fn add_shape_child(a: u32, b: u32, c: u32, d: u32) {
    let state = unsafe { STATE.as_mut() }.expect("got an invalid state pointer");
    let id = uuid_from_u32_quartet(a, b, c, d);
    if let Some(shape) = state.current_shape() {
        shape.children.push(id);
    }
}

#[no_mangle]
pub extern "C" fn clear_shape_children() {
    let state = unsafe { STATE.as_mut() }.expect("got an invalid state pointer");
    if let Some(shape) = state.current_shape() {
        shape.children.clear();
    }
}

#[no_mangle]
pub extern "C" fn add_shape_solid_fill(raw_color: u32) {
    let state = unsafe { STATE.as_mut() }.expect("got an invalid state pointer");
    if let Some(shape) = state.current_shape() {
        let color = skia::Color::new(raw_color);
        shape.add_fill(shapes::Fill::Solid(color));
    }
}

#[no_mangle]
pub extern "C" fn add_shape_linear_fill(
    start_x: f32,
    start_y: f32,
    end_x: f32,
    end_y: f32,
    opacity: f32,
) {
    let state = unsafe { STATE.as_mut() }.expect("got an invalid state pointer");
    if let Some(shape) = state.current_shape() {
        shape.add_fill(shapes::Fill::new_linear_gradient(
            (start_x, start_y),
            (end_x, end_y),
            opacity,
        ))
    }
}

#[derive(Debug)]
pub struct RawStopData {
    color: [u8; 4],
    offset: u8,
}

#[no_mangle]
pub extern "C" fn add_shape_fill_stops(ptr: *mut RawStopData, n_stops: i32) {
    let state = unsafe { STATE.as_mut() }.expect("got an invalid state pointer");
    if let Some(shape) = state.current_shape() {
        unsafe {
            let buf = Vec::<RawStopData>::from_raw_parts(ptr, n_stops as usize, n_stops as usize);
            for raw_stop in buf.iter() {
                let color = skia::Color::from_argb(
                    raw_stop.color[3],
                    raw_stop.color[0],
                    raw_stop.color[1],
                    raw_stop.color[2],
                );
                shape
                    .add_gradient_stop(color, (raw_stop.offset as f32) / 100.)
                    .expect("got no fill or an invalid one");
            }
            mem::free(
                ptr as *mut u8,
                n_stops as usize * std::mem::size_of::<RawStopData>(),
            );
        }
    }
}

#[no_mangle]
pub extern "C" fn store_image(a: u32, b: u32, c: u32, d: u32, ptr: *mut u8, size: u32) {
    if ptr.is_null() || size == 0 {
        panic!("Invalid data, null pointer or zero size");
    }
    let state = unsafe { STATE.as_mut() }.expect("got an invalid state pointer");
    let render_state = state.render_state();
    let id = uuid_from_u32_quartet(a, b, c, d);
    unsafe {
        let image_bytes = Vec::<u8>::from_raw_parts(ptr, size as usize, size as usize);
        let image_data = skia::Data::new_copy(&*image_bytes);
        match Image::from_encoded(image_data) {
            Some(image) => {
                render_state.images.insert(id.to_string(), image);
            }
            None => {
                eprintln!("Error on image decoding");
            }
        }
        mem::free(ptr as *mut u8, size as usize * std::mem::size_of::<u8>());
    }
}

#[no_mangle]
pub extern "C" fn is_image_cached(a: u32, b: u32, c: u32, d: u32) -> bool {
    let state = unsafe { STATE.as_mut() }.expect("got an invalid state pointer");
    let render_state = state.render_state();
    let id = uuid_from_u32_quartet(a, b, c, d);
    render_state.images.contains_key(&id.to_string())
}

#[no_mangle]
pub extern "C" fn add_shape_image_fill(
    a: u32,
    b: u32,
    c: u32,
    d: u32,
    alpha: f32,
    width: f32,
    height: f32,
) {
    let state = unsafe { STATE.as_mut() }.expect("got an invalid state pointer");
    let id = uuid_from_u32_quartet(a, b, c, d);
    if let Some(shape) = state.current_shape() {
        shape.add_fill(shapes::Fill::new_image_fill(
            id,
            (alpha * 0xff as f32).floor() as u8,
            height,
            width,
        ));
    }
}

#[no_mangle]
pub extern "C" fn clear_shape_fills() {
    let state = unsafe { STATE.as_mut() }.expect("got an invalid state pointer");
    if let Some(shape) = state.current_shape() {
        shape.clear_fills();
    }
}

#[no_mangle]
pub extern "C" fn set_shape_blend_mode(mode: i32) {
    let state = unsafe { STATE.as_mut() }.expect("got an invalid state pointer");
    if let Some(shape) = state.current_shape() {
        shape.set_blend_mode(shapes::BlendMode::from(mode));
    }
}

#[no_mangle]
pub extern "C" fn set_shape_opacity(opacity: f32) {
    let state = unsafe { STATE.as_mut() }.expect("got an invalid state pointer");
    if let Some(shape) = state.current_shape() {
        shape.opacity = opacity;
    }
}

fn main() {
    init_gl();
}
