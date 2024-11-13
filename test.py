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
    # Add file existence check
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
    try:
        print("Running positive tests")
        print("Running test: basic print test")
        test_print()
        print("Running test: offset semicolon")
        test_offset_semicolon()
        print("--------------------------------")
        print("Running negative tests")
        print("Running test: missing semicolon")
        test_missing_semicolon()
        print("Running test: wrong extension")
        test_wrong_extension()
    except AssertionError as e:
        print(f"Test failed: {e}")
        exit(1)

def main():
    run_all_tests()

if __name__ == "__main__":
    main()
