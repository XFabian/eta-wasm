use wasmer::{
    imports, Function, FunctionEnv, FunctionEnvMut, Instance, Memory, MemoryView,
    Module, Store, TypedFunction,
};
use std::sync::{Arc, Mutex};
use std::env;
use clap::{Parser, Subcommand};
use env_logger::{Builder, Logger};
use log::{info, debug, warn};
/// Search for a pattern in a file and display the lines that contain it.
#[derive(Parser)]
#[command(name = "ETAllocator")]
#[command(version = "0.1")]
#[command(about = "Custom Allocator for compiled ETA programs to WASM", long_about = None)]
pub struct Cli {
    /// file to load
    pub file: std::path::PathBuf,
    /// Debug flag to trace allocations
    #[arg(short = 'd', long= "debug", default_value = "false")]
    pub debug: bool,
}

/// Environment passed into `eta_malloc` to manage allocation state
#[derive(Clone)]
struct AllocatorEnv {
    memory: Option<Memory>, // populated after instantiation
    heap_ptr: Arc<Mutex<u32>>,
}
// We keep the memory reference here to know if we need to grow the memory
// Later on, when we want to free stuff this becomes useful
fn eta_malloc(mut env: FunctionEnvMut<AllocatorEnv>, size: u32) -> u32 {
    let mut heap = env.data().heap_ptr.lock().unwrap();
    let memory = env.data().memory.as_ref().expect("Memory not initialized");
    // Align to 16 bytes
    *heap = (*heap + 15) & !15;
    debug!("Aligned heap: {}", heap);
    let ptr = *heap;
    let new_ptr = ptr.checked_add(size as u32).expect("overflow") ;
    debug!("Size requested: {} and heap ptr now at: {}", size, new_ptr);
    let mem_bytes = memory.view(&env).data_size() as usize;
    if (new_ptr as usize) >= mem_bytes {
        // let delta_pages = ((new_ptr as usize - mem_bytes) / wasmer::WASM_PAGE_SIZE) + 1;
        // memory.grow(&mut env, delta_pages as u32).expect("memory grow failed");
        panic!("Out of memory: requested {} bytes, only {} available", size, mem_bytes - ptr as usize);
    }
    *heap = new_ptr;
    ptr
}


// Have a debug flag to trace the allocator
// Logs Heap pointer 
// When allocation happens and how many bytes were allocated
// Alos able to specify the function you want to trace
// However, it is able to run your linked wasm module starting from your main function as entry point
// It is not one 
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    let f_path = cli.file;
    let mut builder = Builder::new();
    let builder = if cli.debug{
        builder.filter_module(env!("CARGO_PKG_NAME"), log::LevelFilter::Debug)
    }
    else {
        builder.filter_module(env!("CARGO_PKG_NAME"), log::LevelFilter::Off)
    };
    let _ = builder.try_init();


    let wasm_bytes = std::fs::read(f_path)?;
    let mut store = Store::default();
    let module = Module::new(&store, wasm_bytes)?;

    // Setup environment
    let heap_ptr = Arc::new(Mutex::new(0u32));
    let env = FunctionEnv::new(
        &mut store,
        AllocatorEnv {
            memory: None,
            heap_ptr: heap_ptr.clone(),
        },
    );

    // Define eta_malloc host function
    let eta_malloc_func = Function::new_typed_with_env(&mut store, &env, eta_malloc);

    // Set up imports
    let import_object = imports! {
        "env" => {
            "eta_malloc" => eta_malloc_func, 
        }
    };

    // Instantiate module
    let instance = Instance::new(&mut store, &module, &import_object)?;
    // Access memory and __heap_base
    let memory = instance.exports.get_memory("memory")?.clone();
    let heap_base = instance
        .exports
        .get_global("__heap_base")?
        .get(&mut store)
        .i32()
        .unwrap() as u32;
    debug!("Value of the heap pointer read from module: {:?}", heap_base);
    // Set memory and heap pointer in the env
    {
    let mut env_mut = env.clone().into_mut(&mut store);
    env_mut.data_mut().memory = Some(memory.clone());
    *env_mut.data_mut().heap_ptr.lock().unwrap() = heap_base;
    }
    // Call wasm function that uses malloc
    let alloc_and_ret: TypedFunction<(), i64> =
        instance.exports.get_typed_function(&store, "functoinT")?;
    let ptr = alloc_and_ret.call(&mut store)?;
    println!("Return value: {}", ptr);
    let env_ref = env.clone().into_mut(&mut store);
    println!("Value of the heap pointer: {:?}", env_ref.data().heap_ptr.lock().unwrap());

    let memory = instance.exports.get_memory("memory")?;
    let view: MemoryView = memory.view(&store);
    let mut buf = [0u8; 4];
    view.read(64, &mut buf)?;
    let val = u32::from_le_bytes(buf);
    println!("Value at offset 64: {}", val); // e.g., 1234
    Ok(())
}