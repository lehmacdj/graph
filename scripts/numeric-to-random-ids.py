#!/usr/bin/env python3

import os
import json
import random
import string
import sys

# Predefined ID mappings
predefined_id_map = {
    76: 'AhQufiPzgyRf',
    1: 'pbYxBO6fzBQV',
    881: 'a0fVkm0kR7KE',
    9780: 'S00KkOYoVpFu',
    0: '000000000000'
}

# Function to generate a random alphanumeric ID of length 12
def generate_random_id(length=12):
    characters = string.ascii_letters + string.digits
    return ''.join(random.choice(characters) for _ in range(length))

# Load and update JSON files in the directory
def process_json_files(directory):
    # Dictionary to map old IDs to new IDs
    id_map = predefined_id_map.copy()

    # Read all JSON files to create the ID map
    for filename in os.listdir(directory):
        if filename.endswith('.json'):
            file_path = os.path.join(directory, filename)

            raw_file_id = filename.split('.')[0]
            if raw_file_id == '000000000000':
                continue
            try:
                file_id = int(raw_file_id)
            except ValueError:
                print(f"Invalid file ID: {raw_file_id}")
                continue
            with open(file_path, 'r') as file:
                data = json.load(file)
            data_id = None
            if 'id' in data and isinstance(data['id'], int):
                data_id = data['id']
            assert data_id == file_id, f"ID mismatch: {data_id} != {filename}"
            old_id = data_id

            if old_id not in id_map:
                id_map[old_id] = generate_random_id()

    # Function to replace numeric IDs with new alphanumeric IDs using the ID map
    def replace_ids(data):
        if isinstance(data, dict):
            new_data = {}
            for key, value in data.items():
                if key == 'id' and value in id_map:
                    new_data[key] = id_map[value]
                elif key in ['incoming', 'outgoing'] and isinstance(value, list):
                    for item in value:
                        if item['n'] not in id_map:
                            raise ValueError(f"Invalid ID: {item['n']}")
                    new_data[key] = [
                            {'t': item['t'], 'n': id_map[item['n']]}
                            for item in value
                    ]
                else:
                    raise ValueError(f"Invalid key: {key}")
            return new_data
        else:
            raise ValueError(f"Invalid data type: {type(data)}")

    # Update all JSON files with the new IDs
    for filename in os.listdir(directory):
        if filename.endswith('.json'):
            file_path = os.path.join(directory, filename)

            with open(file_path, 'r') as file:
                data = json.load(file)

            updated_data = replace_ids(data)

            # Save the updated JSON back to the file
            with open(file_path, 'w') as file:
                json.dump(updated_data, file, separators=(',', ':'))

    # Rename the JSON and corresponding .data files with new IDs
    for old_id, new_id in id_map.items():
        old_json_filename = os.path.join(directory, f'{old_id}.json')
        new_json_filename = os.path.join(directory, f'{new_id}.json')
        old_data_filename = os.path.join(directory, f'{old_id}.data')
        new_data_filename = os.path.join(directory, f'{new_id}.data')

        if os.path.exists(old_json_filename):
            os.rename(old_json_filename, new_json_filename)
        if os.path.exists(old_data_filename):
            os.rename(old_data_filename, new_data_filename)

# Check if the directory path is provided as a command line argument
if len(sys.argv) != 2:
    print("Usage: python update_and_rename_ids.py <directory_path>")
    sys.exit(1)

# Directory containing the JSON files
directory = sys.argv[1]

# Process the JSON files
process_json_files(directory)

print("All IDs have been successfully updated and files renamed.")
