# testing the interpreter

import os
import subprocess
import shutil
import platform

def run_doxa(file_path):
    # Determine executable extension based on platform
    exe_ext = '.exe' if platform.system() == 'Windows' else ''
    mer_path = os.path.join('zig-out', 'bin', f'doxa{exe_ext}')
    
    # Use platform-specific command invocation
    if platform.system() == 'Windows':
        command = ['cmd', '/c', mer_path, file_path]
    else:
        command = [mer_path, file_path]
        
    process = subprocess.Popen(command,
                             stdout=subprocess.PIPE, 
                             stderr=subprocess.PIPE,
                             text=True)
    stdout, stderr = process.communicate()
    return stdout.strip(), stderr.strip()

########################################################

# positive tests
def test_print():
    file_path = os.path.join('tests', 'positive', 'p_test_print.doxa')
    if not os.path.exists(file_path):
        raise FileNotFoundError(f"Test file not found: {file_path}")
    
    stdout, stderr = run_doxa(file_path)
    assert stdout == '5', f"Expected '5', but got '{stdout}'"
    print("expected: 5")
    print("got: ", stdout)
    print("✅ test_print passed")

def test_offset_semicolon():
    stdout, stderr = run_doxa(os.path.join('tests', 'positive', 'p_test_offset_semicolon.doxa'))
    assert stdout == '5', f"Expected '5', but got '{stdout}'"
    print("expected: 5")
    print("got: ", stdout)
    print("✅ test_offset_semicolon passed")

def test_math():
    stdout, stderr = run_doxa(os.path.join('tests', 'positive', 'p_test_math.doxa'))
    assert stdout == '5.0', f"Expected '5.0', but got '{stdout}'"
    print("expected: 5.0")
    print("got: ", stdout)
    print("✅ test_math passed")

def test_var_num():
    stdout, stderr = run_doxa(os.path.join('tests', 'positive', 'p_test_var_num.doxa'))
    assert stdout == '5', f"Expected '5', but got '{stdout}'"
    print("expected: 5")
    print("got: ", stdout)
    print("✅ test_var_num passed")

def test_const_num():
    stdout, stderr = run_doxa(os.path.join('tests', 'positive', 'p_test_const_num.doxa'))
    assert stdout == '5', f"Expected '5', but got '{stdout}'"
    print("expected: 5")
    print("got: ", stdout)
    print("✅ test_const_num passed")

def test_var_change():
    stdout, stderr = run_doxa(os.path.join('tests', 'positive', 'p_test_var_change.doxa'))
    assert stdout == '5', f"Expected '5', but got '{stdout}'"
    print("expected: 5")
    print("got: ", stdout)
    print("✅ test_var_change passed")

def test_bracket_scope():
    stdout, stderr = run_doxa(os.path.join('tests', 'positive', 'p_test_bracket_scope.doxa'))
    assert stdout == '5', f"Expected '5', but got '{stdout}'"
    print("expected: 5")
    print("got: ", stdout)
    print("✅ test_bracket_scope passed")

def test_var_str():
    stdout, stderr = run_doxa(os.path.join('tests', 'positive', 'p_test_var_str.doxa'))
    assert stdout == 'five', f"Expected 'five', but got '{stdout}'"
    print("expected: five")
    print("got: ", stdout)
    print("✅ test_var_str passed")

def test_const_float():
    stdout, stderr = run_doxa(os.path.join('tests', 'positive', 'p_test_const_float.doxa'))
    assert stdout == '5.5', f"Expected '5.5', but got '{stdout}'"
    print("expected: 5.5")
    print("got: ", stdout)
    print("✅ test_const_float passed")

def test_div_noremain():
    stdout, stderr = run_doxa(os.path.join('tests', 'positive', 'p_test_div_noremain.doxa'))
    assert stdout == '5.0', f"Expected '5.0', but got '{stdout}'"
    print("expected: 5.0")
    print("got: ", stdout)
    print("✅ test_div_noremain passed")

def test_var_assign_nothing():
    stdout, stderr = run_doxa(os.path.join('tests', 'positive', 'p_test_var_assign_nothing.doxa'))
    assert stdout == 'nothing', f"Expected 'nothing', but got '{stdout}'"
    print("expected: nothing")
    print("got: ", stdout)
    print("✅ test_var_assign_nothing passed")

def test_equality_true():
    stdout, stderr = run_doxa(os.path.join('tests', 'positive', 'p_test_equality_true.doxa'))
    assert stdout == 'true', f"Expected 'true', but got '{stdout}'"
    print("expected: true")
    print("got: ", stdout)
    print("✅ test_equality_true passed")

def test_equality_false():
    stdout, stderr = run_doxa(os.path.join('tests', 'positive', 'p_test_equality_false.doxa'))
    assert stdout == 'false', f"Expected 'false', but got '{stdout}'"
    print("expected: false")
    print("got: ", stdout)
    print("✅ test_equality_false passed")

def test_inequality_true():
    stdout, stderr = run_doxa(os.path.join('tests', 'positive', 'p_test_inequality_true.doxa'))
    assert stdout == 'true', f"Expected 'true', but got '{stdout}'"
    print("expected: true")
    print("got: ", stdout)
    print("✅ test_inequality_true passed")

def test_inequality_false():
    stdout, stderr = run_doxa(os.path.join('tests', 'positive', 'p_test_inequality_false.doxa'))
    assert stdout == 'false', f"Expected 'false', but got '{stdout}'"
    print("expected: false")
    print("got: ", stdout)
    print("✅ test_inequality_false passed")

def test_array():
    stdout, stderr = run_doxa(os.path.join('tests', 'positive', 'p_test_array.doxa'))
    assert stdout == '[1, 2, 3]', f"Expected '[1, 2, 3]', but got '{stdout}'"
    print("expected: [1, 2, 3]")
    print("got: ", stdout)
    print("✅ test_array passed")

########################################################

# negative tests    
def test_missing_semicolon():
    stdout, stderr = run_doxa(os.path.join('tests', 'negetive', 'n_test_w_semicolon.doxa'))
    assert stderr != "", f"Expected error, but got: '{stdout}'"
    print("✅ test_semicolon passed")

def test_wrong_extension():
    stdout, stderr = run_doxa(os.path.join('tests', 'negative', 'n_test_w_ext.met'))
    assert "Error: File must have .doxa extension" in stderr, f"Expected error message about .doxa extension, but got: '{stderr}'"
    print("✅ test_wrong_extension passed")

def test_change_const():
    stdout, stderr = run_doxa(os.path.join('tests', 'negetive', 'n_test_change_const.doxa'))
    assert stderr != "", f"Expected error, but got: '{stdout}'"
    print("✅ test_change_const passed")
    
def test_open_bracket():
    stdout, stderr = run_doxa(os.path.join('tests', 'negetive', 'n_test_open_bracket.doxa'))
    assert stderr != "", f"Expected error, but got: '{stdout}'"
    print("✅ test_open_bracket passed")
    
def test_const_assign_nothing():
    stdout, stderr = run_doxa(os.path.join('tests', 'negetive', 'n_test_const_assign_nothing.doxa'))
    assert stderr != "", f"Expected error, but got: '{stdout}'"
    print("✅ test_const_assign_nothing passed")

def test_comments():
    stdout, stderr = run_doxa(os.path.join('tests', 'positive', 'p_test_comments.doxa'))
    assert stdout == '5', f"Expected '5', but got '{stdout}'"
    print("expected: 5")
    print("got: ", stdout)
    print("✅ test_comments passed")

def test_reassign_string():
    stdout, stderr = run_doxa(os.path.join('tests', 'positive', 'p_test_reassign_string.doxa'))
    assert stdout == 'five', f"Expected 'five', but got '{stdout}'"
    print("expected: five")
    print("got: ", stdout)
    print("✅ test_reassign_string passed")

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
        ("variable string", test_var_str),
        ("constant float", test_const_float),
        ("division no remain", test_div_noremain),
        ("variable assign nothing", test_var_assign_nothing),
        ("comments", test_comments),
        ("equality true", test_equality_true),
        ("equality false", test_equality_false),
        ("inequality false", test_inequality_false),
        ("inequality true", test_inequality_true),
        ("reassign string", test_reassign_string),
    ]
    
    negative_tests = [
        ("missing semicolon", test_missing_semicolon),
        ("wrong extension", test_wrong_extension),
        ("change constant", test_change_const),
        ("open bracket", test_open_bracket),
        ("constant assign nothing", test_const_assign_nothing),
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
        print(f"Build failed:s {e}")
        exit(1)

def main():
    #add titles
    build()
    run_all_tests()

if __name__ == "__main__":
    main()
