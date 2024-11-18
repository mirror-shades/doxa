# testing the interpreter

import os
import subprocess
import shutil
import re

total_tests = 0
passed_tests = 0

def run_passing_numbers():
    # Create a temporary file with all test cases
    temp_file = 'temp_test.doxa'
    with open('test/lexer/numbers_p.txt', 'r') as input_file:
        test_cases = []
        for line in input_file:
            # Skip empty lines and comments
            line = line.strip()
            if not line or line.startswith('#'):
                continue
            test_cases.append([x.strip() for x in line.split(',')])
    
    # Write all test cases to a single file
    with open(temp_file, 'w') as f:
        for test in test_cases:
            f.write(f"{test[0]}\n")
    
    try:
        # Run lexer on the entire file
        process = subprocess.Popen(['./zig-out/bin/doxa', '--debug-lexer', temp_file], 
                                 stdout=subprocess.PIPE, 
                                 stderr=subprocess.PIPE, 
                                 text=True)
        
        stdout, stderr = process.communicate(timeout=5)
        tokens = parse_all_tokens(stderr)
        
        # Match tokens with test cases
        for i, (token, test_case) in enumerate(zip(tokens, test_cases)):
            input_val, expected_type, expected_lexeme, expected_literal = test_case
            print(f"Running test: {input_val} | {expected_type} | {expected_lexeme} | {expected_literal}")
            assert_result(token, expected_type, expected_lexeme, expected_literal)
            
    finally:
        if os.path.exists(temp_file):
            os.remove(temp_file)

def run_all_tests():
    global passed_tests, total_tests
    run_passing_numbers()
    print(f"NUMBERS POSITIVE: Passed {passed_tests} out of {total_tests} tests")
    passed_tests, total_tests = 0, 0

def run_test(input_val, expected_type, expected_lexeme, expected_literal):
    result = lex_string(input_val)
    if result is None:
        print(f"Got EOF token for input: {input_val}")
        return
    assert_result(result, expected_type, expected_lexeme, expected_literal)

def assert_result(result, expected_type, expected_lexeme, expected_literal):
    global total_tests, passed_tests
    total_tests += 1
    
    # Clean up the literal format by removing the leading dot if present
    expected_literal = expected_literal.replace('.', '', 1)
    actual_literal = result[2].strip()
    
    if (result[0] != expected_type or 
        result[1] != expected_lexeme or 
        actual_literal != expected_literal):
        print(f"❌ FAILED: Expected {expected_type} '{expected_lexeme}' {expected_literal}, got {result[0]} '{result[1]}' {actual_literal}")
        return False
    passed_tests += 1
    return True

def lex_string(input_val):
    # Assuming this returns a tuple of (token_type, lexeme, literal) or None for EOF
    result = run_doxa(input_val)
    if result is None:  # EOF token
        return None
    return result

def run_doxa(input_val):
    # Create a temporary file with the input value
    temp_file = 'temp_test.doxa'
    with open(temp_file, 'w') as f:
        f.write(input_val)
    
    process = subprocess.Popen(['./zig-out/bin/doxa', '--debug-lexer', temp_file], 
                             stdout=subprocess.PIPE, 
                             stderr=subprocess.PIPE, 
                             text=True)
    
    try:
        stdout, stderr = process.communicate(timeout=5)
        tokens = parse_all_tokens(stderr)
        if tokens:
            return tokens[0]  # Return the first (and should be only) token
        return None
    except subprocess.TimeoutExpired:
        process.kill()
        print("Process timed out")
    finally:
        # Clean up temp file
        if os.path.exists(temp_file):
            os.remove(temp_file)

def parse_all_tokens(output):
    # Parse all tokens at once and return them as a list of tuples
    pattern = r"Token: token\.TokenType\.(\w+)\s+'([^']+)'\s+token\.TokenLiteral\{\s*\.([^}]+)\s*\}"
    matches = re.findall(pattern, output)
    return matches

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
