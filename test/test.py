# testing the interpreter

import os
import subprocess
import shutil
import re

# global variables
total_tests = 0
total_passed_tests = 0

current_tests = 0
current_passed_tests = 0

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
        print("\nRunning lexer...")
        process = subprocess.Popen(['./zig-out/bin/doxa', '--debug-lexer', temp_file], 
                                 stdout=subprocess.PIPE, 
                                 stderr=subprocess.PIPE, 
                                 text=True)
        
        stdout, stderr = process.communicate(timeout=5)
        tokens = parse_all_tokens(stderr)
        
        # Match tokens with test cases
        for i, (token, test_case) in enumerate(zip(tokens, test_cases)):
            input_val, expected_type, expected_lexeme, expected_literal = test_case
            assert_result(token, expected_type, expected_lexeme, expected_literal)
            
    finally:
        if os.path.exists(temp_file):
            os.remove(temp_file)

def run_passing_strings():
    temp_file = 'temp_test.doxa'
    with open('test/lexer/strings_p.txt', 'r', encoding='utf-8') as input_file:
        test_cases = []
        for line in input_file:
            line = line.strip()
            if not line or line.startswith('#'):
                continue
            
            try:
                # Split on the last occurrence of ", .string = {"
                parts = line.split(', .string = {', 1)
                if len(parts) != 2:
                    continue
                
                front_part, byte_array = parts
                byte_array = byte_array.strip().rstrip('}').strip()
                
                # Find the last occurrence of ", STRING, " in the front part
                last_string_marker = front_part.rfind(', STRING, ')
                if last_string_marker == -1:
                    continue
                
                input_val = front_part[:last_string_marker].strip()
                lexeme = front_part[last_string_marker + len(', STRING, '):].strip()
                
                # Handle quoted values
                if lexeme.startswith('"') and lexeme.endswith('"'):
                    lexeme = lexeme[1:-1]
                
                literal = f"string = {{ {byte_array} }}"
                test_cases.append([input_val, "STRING", lexeme, literal])
                
            except Exception as e:
                print(f"Warning: Failed to parse line: {line}")
                print(f"Error: {e}")
                continue
    
    # Test each string individually
    for test_case in test_cases:
        input_val = test_case[0]
        # Write single test case
        with open(temp_file, 'w', encoding='utf-8') as f:
            if input_val.startswith('r"'):
                # For raw strings, write the content exactly as-is
                content = input_val[1:]  # Just remove the 'r' prefix
                f.write(f"{content}\n")
            else:
                f.write(f"{input_val}\n")
        
        try:
            # Run lexer on single test
            process = subprocess.Popen(['./zig-out/bin/doxa', '--debug-lexer', temp_file], 
                                     stdout=subprocess.PIPE, 
                                     stderr=subprocess.PIPE, 
                                     text=True,
                                     encoding='utf-8')
            
            stdout, stderr = process.communicate(timeout=5)
            if process.returncode != 0:
                print("Lexer error:")
                print(stderr)
                continue
                
            tokens = parse_all_tokens(stderr)
            if not tokens:
                print("No tokens parsed from lexer output")
                continue
                
            # Compare single result
            token = tokens[0]
            expected_type, expected_lexeme, expected_literal = test_case[1:]
            
            # For raw strings, we expect the lexeme to contain the literal backslashes
            if input_val.startswith('r"'):
                expected_lexeme = expected_lexeme.replace('\\', '\\\\')
            
            assert_result(token, expected_type, expected_lexeme, expected_literal)
                
        except Exception as e:
            print(f"Error running test: {e}")
            
    if os.path.exists(temp_file):
        os.remove(temp_file)

def test_comments():
    process = subprocess.Popen(['./zig-out/bin/doxa', '--debug-lexer', './test/lexer/comments.doxa'], 
                             stdout=subprocess.PIPE, 
                             stderr=subprocess.PIPE, 
                             text=True,
                             encoding='utf-8')
    
    try:
        stdout, stderr = process.communicate(timeout=5)
        tokens = parse_all_tokens(stderr)
        if tokens:
            assert_result(tokens[0], "STRING", "hello", "string = { 104, 101, 108, 108, 111 }")

        return None
    except subprocess.TimeoutExpired:
        process.kill()
        print("Process timed out")

def run_test(input_val, expected_type, expected_lexeme, expected_literal):
    result = lex_string(input_val)
    if result is None:
        print(f"Got EOF token for input: {input_val}")
        return
    assert_result(result, expected_type, expected_lexeme, expected_literal)

def assert_result(result, expected_type, expected_lexeme, expected_literal):
    global current_tests, current_passed_tests
    current_tests += 1
    
    # Clean up the literal format by removing the leading dot if present
    expected_literal = expected_literal.replace('.', '', 1)
    actual_literal = result[2].strip()
    
    # For raw strings, we expect the full r"..." format in the lexeme
    if expected_lexeme.startswith('r"'):
        actual_lexeme = result[1]
    else:
        actual_lexeme = result[1]
        if actual_lexeme.startswith('"') and actual_lexeme.endswith('"'):
            actual_lexeme = actual_lexeme[1:-1]
    
    # Check if the literal contains a byte array
    if '{' in expected_literal and '}' in expected_literal:
        expected_bytes = expected_literal.split('{')[1].split('}')[0].strip()
        actual_bytes = actual_literal.split('{')[1].split('}')[0].strip()
        literal_matches = actual_bytes == expected_bytes
    else:
        literal_matches = actual_literal == expected_literal
    
    if (result[0] != expected_type or 
        actual_lexeme != expected_lexeme or 
        not literal_matches):
        print(f"❌ FAILED:")
        if result[0] != expected_type:
            print(f"  Type mismatch: expected {expected_type}, got {result[0]}")
        if actual_lexeme != expected_lexeme:
            print(f"  Lexeme mismatch: expected '{expected_lexeme}', got '{actual_lexeme}'")
        if not literal_matches:
            print(f"  Literal mismatch: expected {expected_literal}, got {actual_literal}")
        return False
    current_passed_tests += 1
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
    with open(temp_file, 'w', encoding='utf-8') as f:
        f.write(input_val)
    
    process = subprocess.Popen(['./zig-out/bin/doxa', '--debug-lexer', temp_file], 
                             stdout=subprocess.PIPE, 
                             stderr=subprocess.PIPE, 
                             text=True,
                             encoding='utf-8')
    
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
    
    # Process each match
    processed_matches = []
    for token_type, lexeme, literal in matches:
        # For raw strings, keep the r" prefix
        if not lexeme.startswith('r"'):
            # Strip surrounding quotes from non-raw string lexemes
            if lexeme.startswith('"') and lexeme.endswith('"'):
                lexeme = lexeme[1:-1]
        
        # Clean up the literal format
        if '{' in literal and '}' in literal:
            # Extract just the byte array part
            literal = 'string = { ' + literal.split('{')[1].split('}')[0].strip() + ' }'
            
        processed_matches.append((token_type, lexeme, literal))
    
    return processed_matches

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

def record_results():
    global total_passed_tests, total_tests, current_passed_tests, current_tests
    total_passed_tests += current_passed_tests
    total_tests += current_tests
    current_passed_tests, current_tests = 0, 0
 
def run_passing_keywords():
    temp_file = 'temp_test.doxa'
    with open('test/lexer/keywords_p.txt', 'r') as input_file:
        test_cases = {}  # Use a dictionary instead of list
        for line in input_file:
            line = line.strip()
            if not line or line.startswith('#'):
                continue
            parts = line.split(',', 3)
            if len(parts) != 4:
                print(f"Warning: Skipping malformed line: {line}")
                continue
            parts = [x.strip() for x in parts]
            # Use the input value as the key
            test_cases[parts[0]] = parts[1:]  # Store just the expected values
    
    # Write all test cases to a single file
    with open(temp_file, 'w') as f:
        for input_val in test_cases.keys():
            f.write(f"{input_val}\n")
    
    try:
        process = subprocess.Popen(['./zig-out/bin/doxa', '--debug-lexer', temp_file], 
                                 stdout=subprocess.PIPE, 
                                 stderr=subprocess.PIPE, 
                                 text=True)
        
        stdout, stderr = process.communicate(timeout=5)
        tokens = parse_all_tokens(stderr)
        
        # Match tokens with test cases using the lexeme as key
        for token in tokens:
            token_lexeme = token[1]
            if token_lexeme in test_cases:
                expected_type, expected_lexeme, expected_literal = test_cases[token_lexeme]
                assert_result(token, expected_type, expected_lexeme, expected_literal)
            else:
                print(f"Warning: Unexpected token: {token}")
            
    finally:
        if os.path.exists(temp_file):
            os.remove(temp_file)

def run_all_tests():
    global current_passed_tests, current_tests

    run_passing_keywords()
    print(f"KEYWORDS POSITIVE: Passed {current_passed_tests} out of {current_tests} tests")
    record_results()

    run_passing_numbers()
    print(f"NUMBERS POSITIVE: Passed {current_passed_tests} out of {current_tests} tests")
    record_results()

    run_passing_strings()
    print(f"STRINGS POSITIVE: Passed {current_passed_tests} out of {current_tests} tests")
    record_results()

    test_comments()
    print(f"COMMENTS POSITIVE: Passed {current_passed_tests} out of {current_tests} tests")
    record_results()

    print(f"TESTING COMPLETE: Passed {total_passed_tests} out of {total_tests} tests")

def main():
    #add titles
    build()
    run_all_tests()

if __name__ == "__main__":
    main()
