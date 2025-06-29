import time
import subprocess
import sys

def run_command(cmd, description):
    print(f"Running {description}")
    print("-" * 50)
    
    start_time = time.time()
    result = subprocess.run(
        cmd,
        capture_output=True,
        text=True
    )
    end_time = time.time()
    
    delta = end_time - start_time
    
    # Print output and errors
    if result.stdout:
        print("Output:")
        print(result.stdout)
    
    if result.stderr:
        print("Errors:")
        print(result.stderr)
    
    print(f"Execution time: {delta:.4f} seconds")
    print(f"Exit code: {result.returncode}")
    print("-" * 50)
    
    return result.returncode == 0, delta

def run_benchmark(file_path):
    print(f"Benchmarking {file_path}")
    print("=" * 60)
    
    # First, print the contents of the file
    print("Test file contents:")
    with open(file_path, 'r') as f:
        print(f.read())
    print("=" * 60)
    
    # Run with deprecated interpreter
    old_success, old_time = run_command(
        ["./zig-out/bin/doxa", "old", file_path],
        "DEPRECATED INTERPRETER (old)"
    )
    
    # Run with new HIR VM
    new_success, new_time = run_command(
        ["./zig-out/bin/doxa", "run", file_path],
        "HIR VM (new)"
    )
    
    # Comparison summary
    print("COMPARISON SUMMARY")
    print("=" * 60)
    print(f"Deprecated Interpreter: {old_time:.4f}s ({'✅ SUCCESS' if old_success else '❌ FAILED'})")
    print(f"HIR VM:                 {new_time:.4f}s ({'✅ SUCCESS' if new_success else '❌ FAILED'})")
    
    if old_success and new_success:
        if new_time < old_time:
            speedup = old_time / new_time
            print(f"🚀 HIR VM is {speedup:.2f}x faster!")
        elif old_time < new_time:
            slowdown = new_time / old_time
            print(f"🐌 HIR VM is {slowdown:.2f}x slower")
        else:
            print("🤷 Performance is roughly equivalent")
    
    return old_success and new_success

def main():
    file_path = "./test/misc/benchmark.doxa"
    if not run_benchmark(file_path):
        sys.exit(1)

if __name__ == "__main__":
    main()