import time

limit = 1_000_000_000


# FizzBuzz benchmark
def fizzbuzzBenchmark():
    startTime = time.time_ns()
    
    for i in range(1, limit + 1):
        if i % 15 == 0:
            "FizzBuzz"
        elif i % 3 == 0:
            "Fizz"
        elif i % 5 == 0:
            "Buzz"
        else:
            str(i)
    
    endTime = time.time_ns()
    print(f"FizzBuzz time: {(endTime - startTime) / 1_000_000_000}s")


# Run all benchmarks
print("=== PYTHON COMPARISON ===")
fizzbuzzBenchmark()
