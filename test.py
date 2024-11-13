# testing the interpreter

import os
import subprocess
import shutil
def run_merve(file_path):
    mer_path = os.path.join('zig-out', 'bin', 'merve.exe')
    process = subprocess.Popen(['cmd', '/c', mer_path, file_path], 
                             stdout=subprocess.PIPE, 
                             stderr=subprocess.PIPE,
                             text=True)
    stdout, stderr = process.communicate()
    return stdout.strip(), stderr.strip()

########################################################

# positive tests
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

def test_var_num():
    stdout, stderr = run_merve('tests/positive/p_test_var_num.mer')
    assert stdout == '5', f"Expected '5', but got '{stdout}'"
    print("✅ test_var_num passed")

def test_const_num():
    stdout, stderr = run_merve('tests/positive/p_test_const_num.mer')
    assert stdout == '5', f"Expected '5', but got '{stdout}'"
    print("✅ test_const_num passed")

def test_var_change():
    stdout, stderr = run_merve('tests/positive/p_test_var_change.mer')
    assert stdout == '5', f"Expected '5', but got '{stdout}'"
    print("✅ test_var_change passed")

def test_bracket_scope():
    stdout, stderr = run_merve('tests/positive/p_test_bracket_scope.mer')
    assert stdout == '5', f"Expected '5', but got '{stdout}'"
    print("✅ test_bracket_scope passed")

########################################################

# negative tests    
def test_missing_semicolon():
    stdout, stderr = run_merve('tests/negetive/n_test_w_semicolon.mer')
    assert stderr != "", f"Expected error, but got: '{stdout}'"
    print("✅ test_semicolon passed")


def test_wrong_extension():
    stdout, stderr = run_merve('tests/negative/n_test_w_ext.met')
    assert "Error: File must have .mer extension" in stderr, f"Expected error message about .mer extension, but got: '{stderr}'"
    print("✅ test_wrong_extension passed")

def test_change_const():
    stdout, stderr = run_merve('tests/negetive/n_test_change_const.mer')
    assert stderr != "", f"Expected error, but got: '{stdout}'"
    print("✅ test_change_const passed")
    
def test_open_bracket():
    stdout, stderr = run_merve('tests/negetive/n_test_open_bracket.mer')
    assert stderr != "", f"Expected error, but got: '{stdout}'"
    print("✅ test_open_bracket passed")

########################################################

def run_all_tests():
    positive_tests = [
        ("basic print test", test_print),
        ("math", test_math),
        ("offset semicolon", test_offset_semicolon),
        ("variable number", test_var_num),
        ("constant number", test_const_num),
        ("variable change", test_var_change),
        ("bracket scope", test_bracket_scope),
    ]
    
    negative_tests = [
        ("missing semicolon", test_missing_semicolon),
        ("wrong extension", test_wrong_extension),
        ("change constant", test_change_const),
        ("open bracket", test_open_bracket),
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
    print("\n--------------------------------\n")
    run_test_suite(negative_tests, "negative")
    
    print(f"\nTests passed: {passed_tests}/{total_tests}")
    if passed_tests < total_tests:
        exit(1)

def build():
    if os.path.exists('zig-out'):
        print("Removing existing build...")
        shutil.rmtree('zig-out')
    print("Building...")
    try:    
        subprocess.run(['zig', 'build'], check=True)
        print("Build successful\n")
    except subprocess.CalledProcessError as e:
        print(f"Build failed: {e}")
        exit(1)

def main():
    #add titles
    build()
    run_all_tests()

if __name__ == "__main__":
    main()
