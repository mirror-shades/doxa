import os
import subprocess
import sys

DIRECTORIES = [
    "macos-x64",
    "macos-arm64",
    "linux-x64",
    "linux-arm64",
    "windows-x64",
]

def run_command(cmd, description=""):
    """Run a command with proper error handling and output"""
    if description:
        print(f"{description}...")
    print(f"Running: {' '.join(cmd)}")
    try:
        result = subprocess.run(cmd, capture_output=False, text=True)
        if result.returncode != 0:
            print(f"Command failed with return code {result.returncode}")
            return False
        return True
    except KeyboardInterrupt:
        print("Command interrupted by user")
        return False
    except Exception as e:
        print(f"Error running command: {e}")
        return False

if not run_command(["zig", "build", "release"], "Building release binaries for all platforms"):
    print("Release build failed")
    sys.exit(1)

print("Release build completed successfully")

def zipReleases(directories):
    for directory in directories:
        print(f"Zipping {directory} release...")
        os.system(f"7z a ./zig-out/{directory}.zip ./zig-out/{directory}/")

zipReleases(DIRECTORIES)
