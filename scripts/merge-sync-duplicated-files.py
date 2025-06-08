#!/usr/bin/env python3

import json
import os
import re
import sys

def dedup_dicts(dict_list):
    """
    Deduplicates a list while preserving the order of elements.
    Even works on lists of dictionaries.
    """
    seen = set()
    deduplicated_list = []

    for d in dict_list:
        # Convert dictionary to a frozenset of its items for hashable comparison
        dict_items = frozenset(d.items())
        if dict_items not in seen:
            seen.add(dict_items)
            deduplicated_list.append(d)

    return deduplicated_list

def merge_json_files(directory):
    """Merges duplicate JSON files in a directory, keeping the union of keys."""
    pattern = re.compile(r"^(.*?)(\s+\d+)?\.json$")  # Match "filename.json" or "filename <number>.json"

    # First pass: collect and group all files by NID
    nid_groups = {}

    for filename in os.listdir(directory):
        match = pattern.match(filename)
        if not match:  # Skip non-matching files
            continue

        nid = match.group(1)  # Extract the NID (base filename without extension)
        file_path = os.path.join(directory, filename)

        if nid not in nid_groups:
            nid_groups[nid] = []
        nid_groups[nid].append((filename, file_path))

    # Second pass: process each NID group
    for nid, files in nid_groups.items():
        if len(files) == 1:  # Handle single files - rename to remove suffix
            filename, file_path = files[0]
            if re.search(r'\s+\d+\.json$', filename):  # Has numeric suffix
                base_filename = nid + ".json"
                base_path = os.path.join(directory, base_filename)
                os.rename(file_path, base_path)
                print(f"Renamed '{filename}' to '{base_filename}'")
            continue
        elif len(files) < 1:  # Skip empty groups
            print(f"No files found for NID '{nid}'")
            continue

        # Read all files in the group
        all_data = []
        for filename, file_path in files:
            with open(file_path, 'r') as f:
                data = json.load(f)
                all_data.append((filename, file_path, data))

        # Verify all files have the same ID
        first_id = all_data[0][2]['id']
        for filename, _, data in all_data:
            assert data['id'] == first_id, f"ID mismatch in {filename}"

        # Merge all data
        merged_incoming = []
        merged_outgoing = []
        for _, _, data in all_data:
            merged_incoming.extend(data.get("incoming", []))
            merged_outgoing.extend(data.get("outgoing", []))

        merged_data = {
            "id": first_id,
            "incoming": dedup_dicts(merged_incoming),
            "outgoing": dedup_dicts(merged_outgoing)
        }

        # Write merged data to the base filename (without number suffix)
        base_filename = nid + ".json"
        base_path = os.path.join(directory, base_filename)

        with open(base_path, 'w') as f:
            json.dump(merged_data, f, separators=(',', ':'))

        # Remove all duplicate files (keep only the base file)
        files_to_remove = []
        for filename, file_path in files:
            if filename != base_filename:
                files_to_remove.append((filename, file_path))

        for filename, file_path in files_to_remove:
            os.remove(file_path)

        if files_to_remove:
            removed_names = [f[0] for f in files_to_remove]
            print(f"Merged {len(files)} files with NID '{nid}' into '{base_filename}' and deleted {removed_names}")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python merge_json.py <directory_path>")
        sys.exit(1)  # Exit with an error code

    directory_path = sys.argv[1]
    if not os.path.isdir(directory_path):
        print(f"Error: '{directory_path}' is not a valid directory.")
        sys.exit(1)

    merge_json_files(directory_path)
