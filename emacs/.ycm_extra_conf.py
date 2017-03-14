# https://github.com/Valloric/ycmd/blob/master/cpp/ycm/.ycm_extra_conf.py
# https://jonasdevlieghere.com/a-better-youcompleteme-config/

import os
import os.path
import logging
import ycm_core

BASE_FLAGS = [
    '-Wall',
    '-std=c++1z',
    '-xc++',
    '-I/usr/lib/'
    '-I/usr/include/'
]

SOURCE_EXTENSIONS = [
    '.cpp',
    '.cxx',
    '.cc',
    '.c',
    '.m',
    '.mm'
]

HEADER_EXTENSIONS = [
    '.h',
    '.hxx',
    '.hpp',
    '.hh'
]

def find_similar_file_in_database(dbpath, filename):
    import json
    import Levenshtein
    logging.info("Trying to find some file close to: " + filename)
    db = json.load(open(dbpath))
    best_filename = ''
    best_distance = 1 << 31
    for entry in db:
        entry_filename = os.path.normpath(
            os.path.join(entry["directory"], entry["file"]))
        distance = Levenshtein.distance(str(filename), str(entry_filename))
        if distance < best_distance:
            best_filename = entry_filename
            best_distance = distance
    return best_filename

def ok_compilation_info(info):
    return bool(info.compiler_flags_)

def get_compilation_info_for_file(dbpath, database, filename):
    info = database.GetCompilationInfoForFile(filename)
    if ok_compilation_info(info):
        logging.info("Flags for file where found in database: " + filename)
        return info
    else:
        logging.info("Flags for file not found in database: " + filename)
        basename = os.path.splitext(filename)[0]
        for extension in SOURCE_EXTENSIONS:
            replacement_file = basename + extension
            logging.info("Trying to replace extension with: " + extension)
            info = database.GetCompilationInfoForFile(replacement_file)
            if ok_compilation_info(info):
                logging.info("Replacing header with: " + replacement_file)
                return info
        replacement_file = find_similar_file_in_database(dbpath, filename)
        logging.info("Replacing header with: " + replacement_file)
        return database.GetCompilationInfoForFile(replacement_file)

def find_nearest(path, target):
    candidates = [
        os.path.join(path, target),
        os.path.join(path, 'build', target),
        os.path.join(path, 'output', target),
    ]
    for candidate in candidates:
        if os.path.isfile(candidate) or os.path.isdir(candidate):
            logging.info("Found nearest " + target + " at " + candidate)
            return candidate
    parent = os.path.dirname(os.path.abspath(path))
    if parent == path:
        raise RuntimeError("Could not find " + target)
    return find_nearest(parent, target)

def make_relative_paths_in_flags_absolute(flags, working_directory):
    if not working_directory:
        return list(flags)
    new_flags = []
    make_next_absolute = False
    path_flags = [ '-isystem', '-I', '-iquote', '--sysroot=' ]
    for flag in flags:
        new_flag = flag
        if make_next_absolute:
            make_next_absolute = False
            if not flag.startswith('/'):
                new_flag = os.path.join(working_directory, flag)
        for path_flag in path_flags:
            if flag == path_flag:
                make_next_absolute = True
                break
            if flag.startswith(path_flag):
                path = flag[ len(path_flag): ]
                new_flag = path_flag + os.path.join(working_directory, path)
                break
        if new_flag:
            new_flags.append(new_flag)
    return new_flags

def flags_for_clang_complete(root):
    try:
        clang_complete_path = find_nearest(root, '.clang_complete')
        clang_complete_flags = open(clang_complete_path, 'r').read().splitlines()
        return clang_complete_flags
    except Exception, err:
        logging.info("Error while looking flags for .clang_complete in root: " + root)
        logging.error(err)
        return None

def flags_for_include(root):
    try:
        include_path = find_nearest(root, 'include')
        flags = []
        for dirroot, dirnames, filenames in os.walk(include_path):
            for dir_path in dirnames:
                real_path = os.path.join(dirroot, dir_path)
                flags = flags + ["-I" + real_path]
        return flags
    except Exception, err:
        logging.info("Error while looking flags for includes in root: " + root)
        logging.error(err)
        return None

def flags_for_compilation_database(root, filename):
    try:
        compilation_db_path = find_nearest(root, 'compile_commands.json')
        compilation_db_dir = os.path.dirname(compilation_db_path)
        logging.info("Set compilation database directory to " + compilation_db_dir)
        compilation_db = ycm_core.CompilationDatabase(compilation_db_dir)
        if not compilation_db:
            logging.info("Compilation database file found but unable to load")
            return None
        compilation_info = get_compilation_info_for_file(
            compilation_db_path, compilation_db, filename)
        if not compilation_info:
            logging.info("No compilation info for " + filename + " in compilation database")
            return None
        return make_relative_paths_in_flags_absolute(
            compilation_info.compiler_flags_,
            compilation_info.compiler_working_dir_)
    except Exception, err:
        logging.info("Error while trying to get flags for " + filename + " in compilation database")
        logging.error(err)
        return None

def flags_for_file(filename):
    root = os.path.realpath(filename)
    compilation_db_flags = flags_for_compilation_database(root, filename)
    if compilation_db_flags:
        final_flags = compilation_db_flags
    else:
        final_flags = BASE_FLAGS
        clang_flags = flags_for_clang_complete(root)
        if clang_flags:
            final_flags = final_flags + clang_flags
        include_flags = flags_for_include(root)
        if include_flags:
            final_flags = final_flags + include_flags
    return {
        'flags': final_flags,
        'do_cache': True
    }

FlagsForFile = flags_for_file
