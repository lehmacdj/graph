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

sequence_ids_node = 'VEfLhuTgZ88Z'

# Function to generate a random alphanumeric ID of length 12
def generate_random_id(length=12):
    characters = string.ascii_letters + string.digits
    return ''.join(random.choice(characters) for _ in range(length))

# Load and update JSON files in the directory
def process_json_files(directory):
    # Dictionary to map old IDs to new IDs
    id_map = predefined_id_map.copy()

    os.makedirs(os.path.join(directory, 'invalid'), exist_ok=True)

    # Read all JSON files to create the ID map
    for filename in os.listdir(directory):
        if filename.endswith('.json'):
            file_path = os.path.join(directory, filename)

            raw_file_id = filename.split('.')[0]
            try:
                file_id = int(raw_file_id)
            except ValueError:
                print(f"Invalid file ID: {raw_file_id}")
                os.rename(file_path, os.path.join(directory, 'invalid', filename))
                continue
            with open(file_path, 'r') as file:
                data = json.load(file)
            if 'id' in data and isinstance(data['id'], int):
                data_id = data['id']
            else:
                print(f"Invalid ID in file: {filename}")
                os.rename(file_path, os.path.join(directory, 'invalid', filename))
                continue
            assert data_id == file_id, f"ID mismatch: {data_id} != {filename}"
            old_id = data_id

            if old_id not in id_map:
                id_map[old_id] = generate_random_id()

    # Function to replace numeric IDs with new alphanumeric IDs using the ID map
    def replace_ids_and_add_sequence_link(data, old_nid):
        if isinstance(data, dict):
            new_data = {}
            for key, value in data.items():
                if key == 'id':
                    if value in id_map:
                        new_data[key] = id_map[value]
                    else:
                        print(f"ID not found in map: {value}")
                        continue
                elif key in ['incoming', 'outgoing'] and isinstance(value, list):
                    new_data[key] = [
                            {'t': item['t'], 'n': id_map[item['n']]}
                            for item in value
                            if item['n'] in id_map
                    ]
                    if key == 'incoming':
                        # add a backlink to a node that contains the original sequence ID
                        new_data[key].append({'t': f"{old_nid}", 'n': sequence_ids_node})
                else:
                    raise ValueError(f"Invalid key: {key}")
            return new_data
        else:
            raise ValueError(f"Invalid data type: {type(data)}")

    # Update all JSON files with the new IDs
    for filename in os.listdir(directory):
        if filename.endswith('.json'):
            file_path = os.path.join(directory, filename)

            raw_file_id = filename.split('.')[0]
            try:
                file_id = int(raw_file_id)
            except ValueError:
                print(f"Invalid file ID: {raw_file_id}")
                os.rename(file_path, os.path.join(directory, 'invalid', filename))
                continue
            with open(file_path, 'r') as file:
                data = json.load(file)
            if 'id' in data and isinstance(data['id'], int):
                data_id = data['id']
            else:
                print(f"Invalid ID in file: {filename}")
                os.rename(file_path, os.path.join(directory, 'invalid', filename))
                continue
            assert data_id == file_id, f"ID mismatch: {data_id} != {filename}"
            old_id = data_id

            with open(file_path, 'r') as file:
                data = json.load(file)

            updated_data = replace_ids_and_add_sequence_link(data, old_id)

            # Save the updated JSON back to the file
            with open(file_path, 'w') as file:
                json.dump(updated_data, file, separators=(',', ':'))

    # create a new sequence ID node that links to all the other nodes with their original sequence id
    sequence_ids_file = os.path.join(directory, f'{sequence_ids_node}.json')
    sequence_ids_data = {
        'id': sequence_ids_node,
        'incoming': [],
        'outgoing': [{'t': f"{old_id}", 'n': id_map[old_id]} for old_id in id_map]
    }
    with open(sequence_ids_file, 'w') as file:
        json.dump(sequence_ids_data, file, separators=(',', ':'))

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
