import os
import re
from io import StringIO

delphi_platform = {
    "windows-win32": "Win32",
    "windows-amd64": "Win64",    
    "android-arm": "Android",
    "android-arm64": "Android64",
    "macos-x86_64": "OSX64",
    "macos-universal2": "OSXARM64",
    "linux-x86_64": "Linux64",
}

android_arch_to_abi = {
    "arm": "armeabi-v7a",
    "arm64": "arm64-v8a",
}

def sort_rule(a):
    if ('windows' in a) and 'win32' in a:
        return (1, a)
    elif ('windows' in a) and 'amd64' in a:
        return (2, a)
    elif ('android' in a) and 'arm' in a:
        return (3, a)
    elif ('android' in a) and ('arm64' in a):
        return (4, a)
    elif ('macos' in a) and ('x86_64' in a):
        return (5, a)
    elif ('macos' in a) and ('universal2' in a):
        return (6, a)
    elif 'linux' in a:
        return (7, a)
    else:
        return (8, a)

def begin(path, version):
    #fh = open(os.path.join(path, version + ".txt"), "w", encoding="cp1252")
    fh = StringIO()
    fh.write(f"FDeployableFiles.Add('{version}', [")        
    return fh

def write(file, small_version, full_version, platform, architecture): 
    offset = "  "
    dplatform = delphi_platform[platform + "-" + architecture]    

    file.write("\n")    
    file.write(f"{offset}//{platform.capitalize()}-{architecture}")    
    file.write("\n")    
    
    if platform == "windows":
        file.write(f"{offset}TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.{dplatform}, 'python\\python3-{platform}-{full_version}-{architecture}.zip', '.\\', True,  True, TDeployOperation.doCopyOnly, ''),")
    elif platform == "android":
        abi = android_arch_to_abi[architecture]
        file.write(f"{offset}TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.{dplatform}, 'python\\python3-{platform}-{full_version}-{architecture}.zip', '.\\assets\\internal', False, True, TDeployOperation.doCopyOnly, ''),")  
        file.write("\n")        
        file.write(f"{offset}TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.{dplatform}, 'python\\{platform}\\{full_version}\\{architecture}\\libpython{small_version}.so', 'library\\lib\\{abi}\\', False, True, TDeployOperation.doSetExecBit, ''),")  
        file.write("\n")
        file.write(f"{offset}TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.{dplatform}, 'python\\{platform}\\{full_version}\\{architecture}\\libpythonlauncher{small_version}.so', 'library\\lib\\{abi}\\', False, True, TDeployOperation.doSetExecBit, ''),")  

        # We must include arm for arm64 too
        if architecture == "arm64":
            abi = android_arch_to_abi["arm"]
            file.write("\n")
            file.write(f"{offset}TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.{dplatform}, 'python\\python3-{platform}-{full_version}-arm.zip', '.\\assets\\internal', False, True, TDeployOperation.doCopyOnly, '''$(AndroidAppBundle)''==''true'''),")  
            file.write("\n")
            file.write(f"{offset}TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.{dplatform}, 'python\\{platform}\\{full_version}\\arm\\libpython{small_version}.so', 'library\\lib\\{abi}\\', False, True, TDeployOperation.doSetExecBit, '''$(AndroidAppBundle)''==''true'''),")  
            file.write("\n")
            file.write(f"{offset}TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.{dplatform}, 'python\\{platform}\\{full_version}\\arm\\libpythonlauncher{small_version}.so', 'library\\lib\\{abi}\\', False, True, TDeployOperation.doSetExecBit, '''$(AndroidAppBundle)''==''true'''),")  

    elif platform == "macos":
        arch_name_folder = "intel"
        if architecture == "universal2":
            arch_name_folder = "arm"
        file.write(f"{offset}TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.{dplatform}, 'python\\python3-{platform}-{full_version}-{architecture}.zip', 'Contents\\Resources\\', True,  True, TDeployOperation.doCopyOnly, ''),")
        file.write("\n")
        file.write(f"{offset}TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.{dplatform}, 'python\\{platform}\\{full_version}\\{arch_name_folder}\\libpython{small_version}.dylib','Contents\\MacOS\\', True,  True, TDeployOperation.doSetExecBit, ''),")
        file.write("\n")
        file.write(f"{offset}TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.{dplatform}, 'python\\{platform}\\{full_version}\\{arch_name_folder}\\python{small_version}','Contents\\MacOS\\', True,  True, TDeployOperation.doSetExecBit, ''),")
    elif platform == "linux": 
        file.write(f"{offset}TPyEnvironmentDeployFile.Create(TPyEnvironmentProjectPlatform.{dplatform}, 'python\\python3-linux-{full_version}-x86_64.zip', '.\\', True,  True, TDeployOperation.doCopyOnly, ''),")

def end(file):
    # Remove the last ,
    val = "".join(file.getvalue().rsplit(",", 1))
    file.seek(0)
    file.write(val)
    file.write("\n")
    file.write("]);")
    file.write("\n\n")
    val = file.getvalue()
    file.close()
    return val

cur_dir = os.path.realpath(os.path.dirname(__file__))

out_file = os.path.join(cur_dir, "deliverables_cmds.txt")
if os.path.exists(out_file):
    os.remove(out_file)

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

with open(out_file, 'w', encoding="cp1252") as fh_out:
    # Create the command by each version
    # We create the version pairs to sort the version set
    ver_pairs = [(float((ver.replace('.', ''))), ver) for ver in ver_set]
    # We use the float key to sort the set
    sorted_ver_pair = sorted(ver_pairs, key=lambda x:x[0])
                          # Using the sorted value (val = original version)
    for small_version in [val for _, val in sorted_ver_pair]:    
        fh = begin(cur_dir, small_version)
        try:        
            re_ver = re.compile(f'python3-.*-{small_version}.*-.*.zip')
            filtered_files = filter(lambda name: (name.startswith('python3-') and name.endswith('.zip')), os.listdir(cur_dir))
            sorted_files = sorted(list(filtered_files), key=sort_rule)
            for file in sorted_files:
                if re_ver.match(file): 
                    platform = re.search(f"python3-(.*)-{small_version}", file).group(1) 
                    architecture = re.search("(.*).zip", file[file.rindex('-')+1:]).group(1)
                    full_version = re.search(f"python3-{platform}-([0-9].[0-9]([0-9])?.*)-{architecture}.zip", file).group(1) 
                    write(fh, small_version, full_version, platform, architecture)
        finally:            
            fh_out.write(end(fh))