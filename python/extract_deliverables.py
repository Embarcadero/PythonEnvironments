# 1) Create the android and macos folders if it doesn't exists
# 2) Scan for android embeddables
#   2.1) For each embeddable, get its version and arch, create a new folder and place the interpreter and launcher files
# 3) Scan for macos embeddables
#   3.1) For each embeddale, get its version and arch, create a new folder and place the interpreter and launcher files

import os
import re
import shutil
from pathlib import Path
from zipfile import ZipFile

# Delete the existing android and macos folders if they exists
def delete_folder(path, platform):
    platform_path = os.path.join(path, platform)
    if os.path.exists(platform_path):
        shutil.rmtree(platform_path)

# Extracts the required files from a given platform/arquitecture
# Creates its folder if needed
def extract_executables(path, file, platform, architecture):    
    # platform
    platform_dir = os.path.join(path, platform)
    os.makedirs(platform_dir, exist_ok=True)
    # Get its version
    version = re.search(f"python3-{platform}-([0-9].[0-9]([0-9])?.*)-{architecture}.zip", file)
    if version:   
        found_ver = version.group(1)        
        ver_dir = os.path.join(platform_dir, found_ver)
        os.makedirs(ver_dir, exist_ok=True)

        if platform == "macos":
            if architecture == "x86_64":
                architecture = "intel"
            else:
                architecture = "arm"

        arch_dir = os.path.join(ver_dir, architecture)
        os.makedirs(arch_dir, exist_ok=True)
        # Open zip file and get the interpreter and launcher files
        with ZipFile(os.path.join(path, file)) as myzipfile:
            #print(myzipfile.namelist())
            major_minor_nums = found_ver.split(".")[0:2]
            ver = major_minor_nums[0] + "." + major_minor_nums[1]            
            if int(major_minor_nums[1]) <= 7:
                ver += "m"

            ext = "so"
            if platform == "macos":
                ext = "dylib"

            with open(os.path.join(arch_dir, f"libpython{ver}.{ext}"), "wb") as target_file:
                target_file.write(myzipfile.read(f"lib/libpython{ver}.{ext}")) 
            
            if platform == "android":
                with open(os.path.join(arch_dir, f"libpythonlauncher{ver}.{ext}"), "wb") as target_file:
                    target_file.write(myzipfile.read(f"bin/python{ver}")) 
            else:
                with open(os.path.join(arch_dir, f"python{ver}"), "wb") as target_file:
                    target_file.write(myzipfile.read(f"bin/python{ver}")) 

cur_dir = os.path.realpath(os.path.dirname(__file__))

delete_folder(cur_dir, "android")
delete_folder(cur_dir, "macos")

# android
arm_regex = re.compile('python3-android-[0-9].[0-9]([0-9])?.*-arm.zip')
arm64_regex = re.compile('python3-android-[0-9].[0-9]([0-9])?.*-arm64.zip')

for root, dirs, files in os.walk(cur_dir):
  for file in files:
    if arm_regex.match(file):        
        extract_executables(cur_dir, file, "android", "arm")
    elif arm64_regex.match(file):
        extract_executables(cur_dir, file, "android", "arm64")

# macOS
arm_regex = re.compile('python3-macos-[0-9].[0-9]([0-9])?.*-x86_64.zip')
arm64_regex = re.compile('python3-macos-[0-9].[0-9]([0-9])?.*-universal2.zip')

for root, dirs, files in os.walk(cur_dir):
  for file in files:
    if arm_regex.match(file):        
        extract_executables(cur_dir, file, "macos", "x86_64")
    elif arm64_regex.match(file):
        extract_executables(cur_dir, file, "macos", "universal2")

