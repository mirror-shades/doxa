# testing the interpreter

import os
import subprocess

def run_merve(file_path):
    mer_path = os.path.join('zig-out', 'bin', 'merve.exe')
    process = subprocess.Popen(['cmd', '/c', mer_path, file_path], 
                             stdout=subprocess.PIPE, 
                             stderr=subprocess.PIPE,
                             text=True)
    stdout, stderr = process.communicate()
    return stdout.strip(), stderr.strip()

def test_print():
    file_path = 'tests/positive/p_test_print.mer'
    if not os.path.exists(file_path):
        raise FileNotFoundError(f"Test file not found: {file_path}")
    
    stdout, stderr = run_merve(file_path)
    assert stdout == '5', f"Expected '5', but got '{stdout}'"
    print("✅ test_print passed")

def test_offset_semicolon():
    stdout, stderr = run_merve('tests/positive/p_test_offset_semicolon.mer')
    assert stdout == '5', f"Expected '5', but got '{stdout}'"
    print("✅ test_offset_semicolon passed")

def test_math():
    stdout, stderr = run_merve('tests/positive/p_test_math.mer')
    assert stdout == '5', f"Expected '5', but got '{stdout}'"
    print("✅ test_math passed")

def test_missing_semicolon():
    stdout, stderr = run_merve('tests/negetive/n_test_w_semicolon.mer')
    assert stderr != "", f"Expected error, but got: '{stdout}'"
    print("✅ test_semicolon passed")


def test_wrong_extension():
    stdout, stderr = run_merve('tests/negative/n_test_w_ext.met')
    assert "Error: File must have .mer extension" in stderr, f"Expected error message about .mer extension, but got: '{stderr}'"
    print("✅ test_wrong_extension passed")

# Optional: Run all tests in the positive directory
def run_all_tests():
    positive_tests = [
        ("basic print test", test_print),
        ("math", test_math),
        ("offset semicolon", test_offset_semicolon),
    ]
    
    negative_tests = [
        ("missing semicolon", test_missing_semicolon),
        ("wrong extension", test_wrong_extension),
    ]
    
    passed_tests = 0
    total_tests = len(positive_tests) + len(negative_tests)
    
    def run_test_suite(test_list, suite_name):
        nonlocal passed_tests
        print(f"Running {suite_name} tests")
        for test_name, test_func in test_list:
            print(f"Running test: {test_name}")
            try:
                test_func()
                passed_tests += 1
            except AssertionError as e:
                print(f"❌ Test failed: {e}")
            except Exception as e:
                print(f"❌ Test error: {e}")
    
    run_test_suite(positive_tests, "positive")
    print("--------------------------------")
    run_test_suite(negative_tests, "negative")
    
    print(f"\nTests passed: {passed_tests}/{total_tests}")
    if passed_tests < total_tests:
        exit(1)

def main():
    run_all_tests()

if __name__ == "__main__":
    main()
