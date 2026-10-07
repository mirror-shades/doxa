import os
import subprocess
import sys
import tarfile
import zipfile

RELEASE_DIR = "doxa"

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

def archive_release(directory):
    source = os.path.join(RELEASE_DIR, directory)
    if not os.path.isdir(source):
        print(f"Skipping {directory}: {source} does not exist")
        return

    label = f"doxa-{directory}"
    root = f"{label}/doxa"

    if directory.startswith("windows"):
        archive = os.path.join(RELEASE_DIR, f"{label}.zip")
        print(f"Zipping {directory} release -> {archive}")
        with zipfile.ZipFile(archive, "w", zipfile.ZIP_DEFLATED) as zf:
            for dirpath, _, files in os.walk(source):
                for name in files:
                    path = os.path.join(dirpath, name)
                    rel = os.path.relpath(path, source).replace(os.sep, "/")
                    zf.write(path, f"{root}/{rel}")
    else:
        archive = os.path.join(RELEASE_DIR, f"{label}.tar.gz")
        print(f"Tarballing {directory} release -> {archive}")
        with tarfile.open(archive, "w:gz") as tf:
            tf.add(source, arcname=root)

def archive_releases(directories):
    for directory in directories:
        archive_release(directory)

def main():
    if not run_command(["zig", "build", "release"], "Building release binaries for all platforms"):
        print("Release build failed")
        return 1
    print("Release build completed successfully")
    archive_releases(DIRECTORIES)
    return 0

if __name__ == "__main__":
    sys.exit(main())