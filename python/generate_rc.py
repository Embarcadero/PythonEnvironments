import os
import shutil
import re

# Delete the existing resources folder if exists
def rebuild_folder(path):
    if os.path.exists(path):
        shutil.rmtree(path)
    os.makedirs(path)

cur_dir = os.path.realpath(os.path.dirname(__file__)) 
resources_dir = os.path.join(cur_dir, 'resources')   

# Sort all versions
ver_set = set([])
for root, dirs, files in os.walk(cur_dir):
  for file in files:
    version = re.search(f"python3-.*-([0-9].[0-9]([0-9])?.*)-.*.zip", file)
    if not version:
        continue
    found_ver = version.group(1)
    major_minor_nums = found_ver.split(".")[0:2]
    ver = major_minor_nums[0] + "." + major_minor_nums[1] 
    ver_set.add(ver)

# Delete the old resources folder if it exists
rebuild_folder(resources_dir) 
# Loop through files of a given version
for small_version in ver_set:                        
    re_ver = re.compile(f'python3-.*-{small_version}.*-.*.zip')
    for _, _, files in os.walk(cur_dir):
        for file in files:
            if re_ver.match(file):    
                platform = re.search(f"python3-(.*)-{small_version}", file).group(1) 
                architecture = re.search("(.*).zip", file[file.rindex('-')+1:]).group(1)
                full_version = re.search(f"python3-{platform}-([0-9].[0-9]([0-9])?.*)-{architecture}.zip", file).group(1)        
                num_version = small_version.replace('.', '')            
                python_version_dir = os.path.join(resources_dir, f"python{num_version}")
                # Creates the python version folder if it doesn't exists
                if not os.path.exists(python_version_dir):
                    os.makedirs(python_version_dir)
                file_name = f"python3-{platform}-{small_version}-{architecture}.rc" 
                with open(os.path.join(python_version_dir, file_name), 'w', encoding='cp1252') as fh:
                    # python38 RCDATA "..\..\python\python3-android-3.8.15-arm64.zip"
                    fh.write(f"python{num_version} RCDATA \"..\\..\\python\\python3-{platform}-{full_version}-{architecture}.zip\"")