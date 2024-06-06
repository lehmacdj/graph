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

    for filename in os.listdir(directory):
        match = pattern.match(filename)
        if not match:  # Skip non-matching files
            continue

        original_name = match.group(1) + ".json"
        original_path = os.path.join(directory, original_name)

        if filename != original_name:  # Skip the original file
            duplicate_path = os.path.join(directory, filename)

            if os.path.exists(original_path):
                # Read and merge JSON content
                with open(original_path, 'r') as f1, open(duplicate_path, 'r') as f2:
                    data1 = json.load(f1)
                    data2 = json.load(f2)
                    assert(data1['id'] == data2['id'])
                    merged_data = {}
                    merged_data["id"] = data1["id"]
                    merged_data["incoming"] = dedup_dicts([v for v in data1["incoming"] + data2["incoming"]])
                    merged_data["outgoing"] = dedup_dicts([v for v in data1["outgoing"] + data2["outgoing"]])

                # Overwrite original file with merged data
                with open(original_path, 'w') as f:
                    json.dump(merged_data, f, separators=(',', ':'))

                # Remove duplicate
                os.remove(duplicate_path)
                print(f"Merged '{filename}' into '{original_name}' and deleted '{filename}'")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python merge_json.py <directory_path>")
        sys.exit(1)  # Exit with an error code

    directory_path = sys.argv[1]
    if not os.path.isdir(directory_path):
        print(f"Error: '{directory_path}' is not a valid directory.")
        sys.exit(1)

    merge_json_files(directory_path)
