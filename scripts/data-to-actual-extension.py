#!/usr/bin/env python3

import os
import subprocess
import re
import argparse
import secrets
import json
import string

file_extensions_id = 'zsBuPkn5mh8F'
mimetypes_id = '927u9Xyky2pC'

def make_empty_special_nodes(directory):
    for nid in [file_extensions_id, mimetypes_id]:
        if not os.path.exists(metadata_path(directory, nid)):
            with open(metadata_path(directory, nid), 'w') as file:
                json.dump({'id': nid, 'incoming': [], 'outgoing': []}, file, separators=(',', ':'))

def find(lst, predicate):
    for item in lst:
        if predicate(item):
            return item
    return None

def metadata_path(directory, nid):
    return os.path.join(directory, '.metadata', f'{nid}.meta.json')

# Function to generate a random alphanumeric ID of length 12
def generate_random_id(length=12):
    characters = string.ascii_letters + string.digits
    return ''.join(secrets.choice(characters) for _ in range(length))

def ensure_transition_from_node(directory, nid, transition_name):
    with open(metadata_path(directory, nid), 'r') as file:
        node = json.load(file)
    transition = find(node['outgoing'], lambda x: x['t'] == transition_name)
    if transition:
        return transition['n']
    else:
        new_id = generate_random_id()
        new_node = {
            'id': new_id,
            'incoming': [{'t': transition_name, 'n': nid}],
            'outgoing': [],
        }
        node['outgoing'].append({'t': transition_name, 'n': new_id})
        with open(metadata_path(directory, new_id), 'w') as file:
            json.dump(new_node, file, separators=(',', ':'))
        with open(metadata_path(directory, nid), 'w') as file:
            json.dump(node, file, separators=(',', ':'))
        return new_id

def add_link(directory, source_nid, transition_name, target_nid):
    with open(metadata_path(directory, source_nid), 'r') as file:
        source = json.load(file)
    with open(metadata_path(directory, target_nid), 'r') as file:
        target = json.load(file)
    outgoing_transition = find(source['outgoing'], lambda x: x['t'] == transition_name)
    if not outgoing_transition:
        source['outgoing'].append({'t': transition_name, 'n': target_nid})
        with open(metadata_path(directory, source_nid), 'w') as file:
            json.dump(source, file, separators=(',', ':'))
    incoming_transition = find(target['incoming'], lambda x: x['t'] == transition_name)
    if not incoming_transition:
        target['incoming'].append({'t': transition_name, 'n': source_nid})
        with open(metadata_path(directory, target_nid), 'w') as file:
            json.dump(target, file, separators=(',', ':'))

def add_filetype_metadata(directory, nid, mimetype, extension, dry_run=True):
    mimetype_parts = mimetype.split('/')
    mimetype_kind = mimetype_parts[0]
    mimetype_subtype = mimetype_parts[1]
    mimetype_kind_id = ensure_transition_from_node(directory, mimetypes_id, mimetype_kind)
    mimetype_subtype_id = ensure_transition_from_node(directory, mimetype_kind_id, mimetype_subtype)
    extension_id = ensure_transition_from_node(directory, file_extensions_id, extension)
    add_link(directory, nid, '', mimetype_subtype_id)
    add_link(directory, nid, '', extension_id)
    add_link(directory, mimetype_subtype_id, 'extension', extension_id)
    add_link(directory, extension_id, 'mimetype', mimetype_subtype_id)

def handle_unidentified_filetype(filepath):
    with open(filepath, 'rb') as file:
        try:
            contents = file.read().decode('utf8')
        except UnicodeDecodeError:
            return 'application/octet-stream', ''
        try:
            json.loads(contents)
            return 'application/json', '.json'
        except json.JSONDecodeError:
            pass
        if re.match(r'^.{0,20}<!DOCTYPE html', contents):
            # must match html first because it can also match the xml regex
            return 'text/html', '.html'
        if re.match(r'^\s{0,20}<\?xml', contents):
            return 'application/xml', '.xml'
        if re.match(r'^[a-z]{1,10}://', contents):
            return 'text/uri-list', '.uri'
        return 'text/plain', '.txt'

def get_mimetype(filepath):
    try:
        # Use file command with --mime-type option
        result = subprocess.run(
            ['file', '--brief', '--mime-type', filepath],
            capture_output=True,
            text=True
        )
        return result.stdout.strip()
    except subprocess.CalledProcessError:
        return None

def get_file_extension(filepath):
    try:
        # Use file command with --extension option
        result = subprocess.run(
            ['file', '--extension', filepath],
            capture_output=True,
            text=True
        )
        output = result.stdout.strip()

        # Extract the extension from the output
        # multiple extensions can be separated by '/', e.g. jpeg/jpg/jpe/jfif
        match = re.match(r'((?:[a-zA-Z0-9]+/)*[a-zA-Z0-9]+)', output)
        if not match:
            return None
        extensions = match.group(1).split('/')
        return f".{extensions[0]}"
    except subprocess.CalledProcessError:
        return None

extension_lookup_table = {
    'video/mp4': '.mp4',
}

def get_mimetype_and_extension(filepath):
    mimetype = get_mimetype(filepath)
    extension = get_file_extension(filepath)
    if mimetype != None and extension != None:
        return mimetype, extension
    elif mimetype != None and extension == None and mimetype in extension_lookup_table:
        return mimetype, extension_lookup_table[mimetype]
    elif mimetype == None and extension != None:
        print(f"[WARNING] {filepath}: identified extension {extension} but no mimetype")
        return 'application/octet-stream', extension
    else:
        mimetype_, extension_ = handle_unidentified_filetype(filepath)
        if mimetype != None and mimetype != mimetype_:
            print(f"[WARNING] {filepath}: identified different mimetype {mimetype_} than file did {mimetype}")
        return mimetype_, extension_

def rename_file(filepath, new_filepath, dry_run=True):
    if filepath == new_filepath:
        print(f"Skipping move for: {filepath}")
    elif dry_run:
        print(f"Would rename: {filepath} -> {new_filepath}")
    elif filepath != new_filepath:
        os.rename(filepath, new_filepath)
        print(f"Renamed: {filepath} -> {new_filepath}")

def rename_data_files(directory, dry_run=True):
    for filename in os.listdir(directory):
        if not filename.endswith('.data'):
            continue
        nid = filename.split('.')[0]

        filepath = os.path.join(directory, filename)
        mimetype, new_extension = get_mimetype_and_extension(filepath)
        new_filename = re.sub(r'\.data$', new_extension, filename)
        new_filepath = os.path.join(directory, new_filename)
        rename_file(filepath, new_filepath, dry_run=dry_run)
        print(f"Adding metadata for {nid}: mimetype: {mimetype}, extension: {new_extension}")
        add_filetype_metadata(directory, nid, mimetype, new_extension, dry_run=dry_run)

def organize_metadata_into_metadata_folder(directory, dry_run=True):
    metadata_folder = os.path.join(directory, '.metadata')
    if not os.path.exists(metadata_folder) and not dry_run:
        os.makedirs(metadata_folder)
    for filename in os.listdir(directory):
        if not filename.endswith('.json'):
            continue

        filepath = os.path.join(directory, filename)
        new_filename = re.sub(r'\.json$', '.meta.json', filename)
        new_filepath = os.path.join(metadata_folder, new_filename)
        rename_file(filepath, new_filepath, dry_run=dry_run)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Rename files ending in data with their actual extension')
    parser.add_argument('--dry-run', action='store_true', help='Print the changes without renaming the files')
    parser.add_argument('directory', help='The directory containing the files to rename')
    args = parser.parse_args()
    organize_metadata_into_metadata_folder(args.directory, dry_run=False)
    make_empty_special_nodes(args.directory)
    rename_data_files(args.directory, dry_run=False)
