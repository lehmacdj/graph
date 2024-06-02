#!/usr/bin/env python3

import os
import json
import random
import string
import sys

# Function to generate a random alphanumeric ID of length 12
def generate_random_id(length=12):
    characters = string.ascii_letters + string.digits
    return ''.join(random.choice(characters) for _ in range(length))

# Load and update JSON files in the directory
def process_json_files(directory):
    # Dictionary to map old IDs to new IDs
    id_map = {}

    # Read all JSON files to create the ID map
    for filename in os.listdir(directory):
        if filename.endswith('.json'):
            file_path = os.path.join(directory, filename)

            with open(file_path, 'r') as file:
                data = json.load(file)

            # Map the top-level ID
            if 'id' in data and isinstance(data['id'], int):
                old_id = data['id']
                if old_id not in id_map:
                    id_map[old_id] = generate_random_id()

            # Map IDs in incoming and outgoing lists
            for link_type in ['incoming', 'outgoing']:
                if link_type in data and isinstance(data[link_type], list):
                    for item in data[link_type]:
                        if 'n' in item and isinstance(item['n'], int):
                            old_id = item['n']
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
                    new_data[key] = [{'t': item['t'], 'n': id_map[item['n']]} if item['n'] in id_map else item for item in value]
                else:
                    new_data[key] = replace_ids(value)
            return new_data
        elif isinstance(data, list):
            return [replace_ids(item) for item in data]
        else:
            return data

    # Update all JSON files with the new IDs
    for filename in os.listdir(directory):
        if filename.endswith('.json'):
            file_path = os.path.join(directory, filename)

            with open(file_path, 'r') as file:
                data = json.load(file)

            updated_data = replace_ids(data)

            # Save the updated JSON back to the file
            with open(file_path, 'w') as file:
                json.dump(updated_data, file, indent=4)

    # Rename the files with new IDs
    for old_id, new_id in id_map.items():
        old_filename = os.path.join(directory, f'{old_id}.json')
        new_filename = os.path.join(directory, f'{new_id}.json')
        if os.path.exists(old_filename):
            os.rename(old_filename, new_filename)

# Check if the directory path is provided as a command line argument
if len(sys.argv) != 2:
    print("Usage: python update_and_rename_ids.py <directory_path>")
    sys.exit(1)

# Directory containing the JSON files
directory = sys.argv[1]

# Process the JSON files
process_json_files(directory)

print("All IDs have been successfully updated and files renamed.")
