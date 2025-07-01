import time
import subprocess
import sys
import os
import argparse
import json
import datetime

# Default filename for benchmark records (in the same directory as this script)
BENCHMARK_RESULTS_FILE = os.path.join(os.path.dirname(__file__), "benchmark_results.json")

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

def save_benchmark_record(results, record_file):
    """Save benchmark results to a JSON file."""
    timestamp = datetime.datetime.now().isoformat()
    
    # Prepare the record data
    record = {
        "timestamp": timestamp,
        "benchmarks": []
    }
    
    total_old_time = 0
    total_new_time = 0
    successful_benchmarks = 0
    
    for name, success, old_time, new_time in results:
        benchmark_record = {
            "name": name,
            "success": success,
            "deprecated_interpreter_time": old_time,
            "hir_vm_time": new_time
        }
        
        if success:
            speedup = old_time / new_time if new_time > 0 else float('inf')
            benchmark_record["speedup"] = speedup
            total_old_time += old_time
            total_new_time += new_time
            successful_benchmarks += 1
        else:
            benchmark_record["speedup"] = None
        
        record["benchmarks"].append(benchmark_record)
    
    # Add summary
    if successful_benchmarks > 0:
        overall_speedup = total_old_time / total_new_time if total_new_time > 0 else float('inf')
        record["summary"] = {
            "total_deprecated_time": total_old_time,
            "total_hir_vm_time": total_new_time,
            "overall_speedup": overall_speedup,
            "successful_benchmarks": successful_benchmarks,
            "total_benchmarks": len(results)
        }
    else:
        record["summary"] = {
            "total_deprecated_time": 0,
            "total_hir_vm_time": 0,
            "overall_speedup": None,
            "successful_benchmarks": 0,
            "total_benchmarks": len(results)
        }
    
    # Load existing records if file exists, otherwise start with empty list
    records = []
    if os.path.exists(record_file):
        try:
            with open(record_file, 'r') as f:
                records = json.load(f)
        except (json.JSONDecodeError, IOError):
            # If file is corrupted or unreadable, start fresh
            records = []
    
    # Add new record
    records.append(record)
    
    # Save back to file
    try:
        with open(record_file, 'w') as f:
            json.dump(records, f, indent=2)
        print(f"📝 Benchmark results saved to {record_file}")
    except IOError as e:
        print(f"❌ Failed to save benchmark results: {e}")

def show_benchmark_history(record_file):
    """Display benchmark history from a JSON file."""
    if not os.path.exists(record_file):
        print(f"❌ Record file {record_file} does not exist")
        return
    
    try:
        with open(record_file, 'r') as f:
            records = json.load(f)
    except (json.JSONDecodeError, IOError) as e:
        print(f"❌ Failed to read record file: {e}")
        return
    
    if not records:
        print("📊 No benchmark records found")
        return
    
    print(f"📊 BENCHMARK HISTORY ({len(records)} records)")
    print("=" * 80)
    print(f"{'Date/Time':<20} {'Overall Speedup':<15} {'Successful':<12} {'Total Time (s)'}")
    print("-" * 80)
    
    for record in records[-10:]:  # Show last 10 records
        timestamp = record.get('timestamp', 'Unknown')
        # Format timestamp to be more readable
        try:
            dt = datetime.datetime.fromisoformat(timestamp.replace('Z', '+00:00'))
            formatted_time = dt.strftime('%Y-%m-%d %H:%M')
        except:
            formatted_time = timestamp[:16]  # Fallback
        
        summary = record.get('summary', {})
        speedup = summary.get('overall_speedup')
        successful = summary.get('successful_benchmarks', 0)
        total = summary.get('total_benchmarks', 0)
        total_time = summary.get('total_hir_vm_time', 0)
        
        speedup_str = f"{speedup:.2f}x" if speedup else "N/A"
        success_str = f"{successful}/{total}"
        
        print(f"{formatted_time:<20} {speedup_str:<15} {success_str:<12} {total_time:<.4f}")
    
    if len(records) > 10:
        print(f"... (showing last 10 of {len(records)} records)")
    
    # Show latest results in detail
    if records:
        latest = records[-1]
        print(f"\n🔍 LATEST BENCHMARK DETAILS ({latest.get('timestamp', 'Unknown')})")
        print("-" * 60)
        for bench in latest.get('benchmarks', []):
            name = bench.get('name', 'Unknown')
            success = '✅' if bench.get('success', False) else '❌'
            speedup = bench.get('speedup')
            speedup_str = f"{speedup:.2f}x" if speedup else "N/A"
            print(f"{success} {name:<20} {speedup_str}")

def main():
    # Parse command line arguments
    parser = argparse.ArgumentParser(
        description="Benchmark suite for DOXA interpreters",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python benchmark.py          # Run benchmarks normally
  python benchmark.py --store  # Run benchmarks and save results
  python benchmark.py --show   # Show benchmark history
        """
    )
    parser.add_argument(
        '--store', 
        action='store_true',
        help='Save benchmark results to benchmark_results.json'
    )
    parser.add_argument(
        '--show',
        action='store_true',
        help='Show benchmark history from benchmark_results.json'
    )
    
    args = parser.parse_args()
    
    # Handle --show command
    if args.show:
        show_benchmark_history(BENCHMARK_RESULTS_FILE)
        return
    
    # Define benchmark files
    benchmarks = [
        ("./test/benchmark/fizzbuzz_benchmark.doxa", "FizzBuzz Loop"),
        ("./test/benchmark/fibonacci_benchmark.doxa", "Fibonacci Recursive"),
        ("./test/benchmark/prime_benchmark.doxa", "Prime Numbers"),
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
    if args.store:
        print(f"📝 Results will be saved to: {BENCHMARK_RESULTS_FILE}")
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
    
    # Save results to file if --store flag is provided
    if args.store:
        save_benchmark_record(results, BENCHMARK_RESULTS_FILE)
    
    if not all_successful:
        sys.exit(1)

if __name__ == "__main__":
    main()