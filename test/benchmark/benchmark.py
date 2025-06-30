import time
import subprocess
import sys
import os

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

def run_benchmark(file_path, benchmark_name):
    print(f"Benchmarking {benchmark_name}: {file_path}")
    print("=" * 60)
    
    # First, print the contents of the file
    print("Test file contents:")
    with open(file_path, 'r') as f:
        print(f.read())
    print("=" * 60)
    
    # Run with deprecated interpreter
    old_success, old_time = run_command(
        ["./zig-out/bin/doxa", "old", file_path],
        f"{benchmark_name} - DEPRECATED INTERPRETER (old)"
    )
    
    # Run with new HIR VM
    new_success, new_time = run_command(
        ["./zig-out/bin/doxa", "run", file_path],
        f"{benchmark_name} - HIR VM (new)"
    )
    
    # Comparison summary
    print(f"{benchmark_name} COMPARISON SUMMARY")
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
    
    print("\n")
    return old_success and new_success, old_time, new_time

def main():
    # Define benchmark files
    benchmarks = [
        ("./test/benchmark/fizzbuzz_benchmark.doxa", "FizzBuzz Loop"),
        ("./test/benchmark/fibonacci_benchmark.doxa", "Fibonacci Recursive"),
        # ("./test/benchmark/prime_benchmark.doxa", "Prime Numbers"),
    ]
    
    # Check if all benchmark files exist
    missing_files = []
    for file_path, _ in benchmarks:
        if not os.path.exists(file_path):
            missing_files.append(file_path)
    
    if missing_files:
        print("❌ Missing benchmark files:")
        for file in missing_files:
            print(f"  - {file}")
        sys.exit(1)
    
    print("🚀 DOXA INTERPRETER BENCHMARK SUITE")
    print("=" * 60)
    print("Running comprehensive performance comparison between:")
    print("  • Deprecated Interpreter (old)")
    print("  • HIR VM (new)")
    print("=" * 60)
    print()
    
    results = []
    all_successful = True
    
    # Run each benchmark
    for file_path, benchmark_name in benchmarks:
        success, old_time, new_time = run_benchmark(file_path, benchmark_name)
        results.append((benchmark_name, success, old_time, new_time))
        if not success:
            all_successful = False
    
    # Overall summary
    print("🏁 OVERALL BENCHMARK RESULTS")
    print("=" * 80)
    print(f"{'Benchmark':<20} {'Old (s)':<10} {'New (s)':<10} {'Speedup':<10} {'Status'}")
    print("-" * 80)
    
    total_old_time = 0
    total_new_time = 0
    successful_benchmarks = 0
    
    for name, success, old_time, new_time in results:
        if success:
            speedup = old_time / new_time if new_time > 0 else float('inf')
            speedup_str = f"{speedup:.2f}x" if speedup != float('inf') else "∞"
            status = "✅ PASS"
            total_old_time += old_time
            total_new_time += new_time
            successful_benchmarks += 1
        else:
            speedup_str = "N/A"
            status = "❌ FAIL"
        
        print(f"{name:<20} {old_time:<10.4f} {new_time:<10.4f} {speedup_str:<10} {status}")
    
    print("-" * 80)
    
    if successful_benchmarks > 0:
        overall_speedup = total_old_time / total_new_time if total_new_time > 0 else float('inf')
        print(f"{'TOTAL':<20} {total_old_time:<10.4f} {total_new_time:<10.4f} {overall_speedup:.2f}x")
        print()
        print(f"📊 SUMMARY: HIR VM is {overall_speedup:.2f}x faster overall!")
        print(f"   Successful benchmarks: {successful_benchmarks}/{len(benchmarks)}")
    
    if not all_successful:
        sys.exit(1)

if __name__ == "__main__":
    main()