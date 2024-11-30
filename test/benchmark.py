import time
import subprocess
import sys

def run_benchmark(file_path):
    print(f"Running benchmark on {file_path}")
    print("-" * 50)
    
    # First, print the contents of the file
    print("Test file contents:")
    with open(file_path, 'r') as f:
        print(f.read())
    print("-" * 50)
    
    # Run with timing
    start_time = time.time()
    result = subprocess.run(
        ["./zig-out/bin/doxa", file_path],
        capture_output=True,
        text=True
    )
    end_time = time.time()
    
    # Print output and errors
    if result.stdout:
        print("Output:")
        print(result.stdout)
    
    if result.stderr:
        print("Errors:")
        print(result.stderr)
    
    delta = end_time - start_time
    print(f"Execution time: {delta:.4f} seconds")
    print(f"Exit code: {result.returncode}")
    
    return result.returncode == 0

def main():
    file_path = "./test/misc/benchmark.doxa"
    if not run_benchmark(file_path):
        sys.exit(1)

if __name__ == "__main__":
    main()