import pygame
import sys
import math
import random
import os
import time

# Initialize Pygame
pygame.init()

# Enhanced Constants - NEW LAYOUT - OPTIMIZED SIZE
TILE_SIZE = 40  # Size of each tile in pixels
MAP_SIZE = 16  # Size of the map in tiles (16x16)
PLAYER_PANEL_WIDTH = 220  # Left panel for player stats
POWERUP_PANEL_HEIGHT = 140  # Bottom panel for power-ups - increased height
WINDOW_WIDTH = PLAYER_PANEL_WIDTH + MAP_SIZE * TILE_SIZE + 20  # Player panel + map + margin
WINDOW_HEIGHT = MAP_SIZE * TILE_SIZE + POWERUP_PANEL_HEIGHT + 20  # Map + power-ups + margin
# print("window size:", WINDOW_WIDTH, "x", WINDOW_HEIGHT)
MIN_WINDOW_WIDTH = 800  # Minimum width for the window
MIN_WINDOW_HEIGHT = 600  # Minimum height for the window
FPS = 60  # Frames per second for the game loop

# Layout offsets
MAP_OFFSET_X = PLAYER_PANEL_WIDTH + 10  # Map starts after player panel
MAP_OFFSET_Y = 10  # Small top margin
POWERUP_OFFSET_Y = MAP_OFFSET_Y + MAP_SIZE * TILE_SIZE + 10  # Power-ups below map

# Professional Color Palette
COLORS = {
    # Floor with gradient effect
    'FLOOR_LIGHT': (245, 235, 205),
    'FLOOR_MID': (230, 220, 190),
    'FLOOR_DARK': (215, 205, 175),
    'FLOOR_SHADOW': (200, 190, 160),

    # Enhanced backgrounds
    'BACKGROUND': (45, 55, 65),
    'UI_BACKGROUND': (35, 45, 55),
    'PANEL_BG': (25, 35, 45),

    # Text with glow
    'TEXT_WHITE': (255, 255, 255),
    'TEXT_GOLD': (255, 215, 0),
    'TEXT_SHADOW': (0, 0, 0),
    'TEXT_CYAN': (100, 255, 255),
    'TEXT_ORANGE': (255, 165, 0),

    # Enhanced brick walls
    'BRICK_TOP': (180, 90, 45),
    'BRICK_MID': (160, 80, 40),
    'BRICK_DARK': (140, 70, 35),
    'BRICK_SHADOW': (120, 60, 30),
    'MORTAR': (100, 50, 25),

    # Beautiful wooden barrels
    'WOOD_LIGHT': (200, 140, 90),
    'WOOD_MID': (180, 120, 70),
    'WOOD_DARK': (160, 100, 50),
    'WOOD_SHADOW': (140, 80, 30),
    'WOOD_HIGHLIGHT': (220, 160, 110),
    'WOOD_BAND': (100, 60, 30),

    # Shiny metal barrels
    'METAL_LIGHT': (160, 165, 170),
    'METAL_MID': (130, 135, 140),
    'METAL_DARK': (100, 105, 110),
    'METAL_SHADOW': (70, 75, 80),
    'METAL_SHINE': (200, 205, 210),
    'METAL_BAND': (60, 65, 70),

    # Vibrant player colors
    'PLAYER_1': (80, 150, 255),  # Bright Blue
    'PLAYER_2': (255, 80, 100),  # Bright Red
    'PLAYER_3': (80, 220, 120),  # Bright Green
    'PLAYER_4': (255, 200, 80),  # Bright Yellow
    'SKIN': (255, 220, 180),
    'SKIN_SHADOW': (230, 195, 155),

    # Glowing power-ups
    'POWERUP_GLOW': (255, 255, 150),
    'POWERUP_CORE': (255, 215, 0),
    'POWERUP_PULSE': (255, 255, 100),

    # Special effects
    'SHADOW': (0, 0, 0, 60),
    'HIGHLIGHT': (255, 255, 255, 100),
    'SELECTION': (255, 255, 0, 150),
    'GRID_LINE': (0, 0, 0, 40),
}


class EnhancedMapVisualizer:
    def __init__(self):
        # Make window resizable and start with smaller size
        initial_width = min(WINDOW_WIDTH, 900)
        initial_height = min(WINDOW_HEIGHT, 700)
        self.screen = pygame.display.set_mode((initial_width, initial_height), pygame.RESIZABLE)
        pygame.display.set_caption("🎮 Playing with Fire 2 Map (Resizable)")
        self.clock = pygame.time.Clock()

        # Current window dimensions
        self.current_width = initial_width
        self.current_height = initial_height
        self.scale_factor = min(initial_width / WINDOW_WIDTH, initial_height / WINDOW_HEIGHT)

        # Fonts
        self.title_font = pygame.font.Font(None, 36)
        self.font = pygame.font.Font(None, 24)
        self.small_font = pygame.font.Font(None, 18)
        self.powerup_font = pygame.font.Font(None, 20)

        # Animation variables
        self.time = 0
        self.powerup_pulse = 0
        self.camera_shake = 0
        self.selected_tile = None

        # Dynamic reload system
        self.last_file_time = 0
        self.check_file_changes()  # Initialize file time

        # Game state tracking for animations
        self.previous_map_data = None
        self.player_animations = {}  # Track player movement animations
        self.explosion_animations = []  # Active explosion effects
        self.bomb_animations = {}  # Bomb countdown animations
        self.powerup_animations = []  # Power-up pickup effects
        self.game_effects = []  # General effect animations

        # Animation timing
        self.last_reload_check = time.time()
        self.reload_check_interval = 0.5  # Check for file changes every 0.5 seconds

        # Load map data and player stats
        self.map_data = self.load_test_data()
        self.player_stats = self.load_player_stats()

        # Create surfaces for smooth rendering (will be scaled)
        self.map_surface = pygame.Surface((MAP_SIZE * TILE_SIZE, MAP_SIZE * TILE_SIZE))
        self.player_panel_surface = pygame.Surface((PLAYER_PANEL_WIDTH, MAP_SIZE * TILE_SIZE))
        self.powerup_panel_surface = pygame.Surface((WINDOW_WIDTH, POWERUP_PANEL_HEIGHT))

        # Virtual surface for full layout
        self.virtual_surface = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT))

    def handle_window_resize(self, new_width, new_height):
        """Handle window resizing"""
        self.current_width = max(new_width, MIN_WINDOW_WIDTH)
        self.current_height = max(new_height, MIN_WINDOW_HEIGHT)

        # Update scale factor to maintain aspect ratio
        self.scale_factor = min(
            self.current_width / WINDOW_WIDTH,
            self.current_height / WINDOW_HEIGHT
        )

        # Recreate screen surface
        self.screen = pygame.display.set_mode((self.current_width, self.current_height), pygame.RESIZABLE)

    def parse_erlang_map_file(self, filename):
        """Parse Erlang map file and extract grid data"""
        try:
            with open(filename, 'r') as file:
                content = file.read()

            # Find the get_map() function
            start_marker = "get_map() ->"
            end_marker = "]."

            start_pos = content.find(start_marker)
            if start_pos == -1:
                raise ValueError("Could not find get_map() function in file")

            # Find the opening bracket of the list
            list_start = content.find("[", start_pos)
            if list_start == -1:
                raise ValueError("Could not find opening bracket of map data")

            # Find the closing bracket and period
            end_pos = content.find(end_marker, list_start)
            if end_pos == -1:
                raise ValueError("Could not find end of map data")

            # Extract the map data string
            map_data_str = content[list_start:end_pos + 1]
            print(f"🔍 Extracted Erlang data: {map_data_str[:200]}...")

            # Parse the Erlang list syntax
            result = self.parse_erlang_list(map_data_str)
            print(f"✅ Successfully parsed {len(result) if result else 0} rows from Erlang file")
            return result

        except FileNotFoundError:
            print(f"⚠️ Could not find file: {filename}")
            print("📝 Using fallback test data...")
            return self.get_fallback_grid()
        except Exception as e:
            print(f"⚠️ Error parsing Erlang file: {e}")
            print("📝 Using fallback test data...")
            return self.get_fallback_grid()

    def parse_erlang_list(self, erlang_str):
        """Parse Erlang list syntax to Python list using a proper tokenizer approach"""
        try:
            # Clean up the string but preserve structure
            clean_str = erlang_str.strip()

            # Remove outer brackets and period if present
            if clean_str.startswith('[') and clean_str.endswith('].'):
                clean_str = clean_str[1:-2]
            elif clean_str.startswith('[') and clean_str.endswith(']'):
                clean_str = clean_str[1:-1]

            # Now we need to parse rows - each row is [tuple, tuple, tuple, ...]
            rows = []
            i = 0

            while i < len(clean_str):
                # Skip whitespace and commas between rows
                while i < len(clean_str) and clean_str[i] in ' \n\t,':
                    i += 1

                if i >= len(clean_str):
                    break

                # Expect a '[' to start a row
                if clean_str[i] == '[':
                    row_start = i
                    bracket_count = 1
                    i += 1

                    # Find the matching closing bracket
                    while i < len(clean_str) and bracket_count > 0:
                        if clean_str[i] == '[':
                            bracket_count += 1
                        elif clean_str[i] == ']':
                            bracket_count -= 1
                        i += 1

                    # Extract the row content (without outer brackets)
                    row_content = clean_str[row_start + 1:i - 1]

                    # Parse this row
                    row = self.parse_erlang_row(row_content)
                    if row:
                        rows.append(row)
                else:
                    # Skip unexpected characters
                    i += 1

            # Validate the structure
            if self.validate_grid_structure(rows):
                return rows
            else:
                print("⚠️ Parsed grid structure is invalid, using fallback")
                return self.get_fallback_grid()

        except Exception as e:
            print(f"⚠️ Error parsing Erlang list: {e}")
            return self.get_fallback_grid()

    def parse_erlang_row(self, row_str):
        """Parse a single row of tuples from Erlang syntax"""
        try:
            row = []
            i = 0

            while i < len(row_str):
                # Skip whitespace and commas
                while i < len(row_str) and row_str[i] in ' \n\t,':
                    i += 1

                if i >= len(row_str):
                    break

                # Expect a '{' to start a tuple
                if row_str[i] == '{':
                    tuple_start = i
                    brace_count = 1
                    i += 1

                    # Find the matching closing brace
                    while i < len(row_str) and brace_count > 0:
                        if row_str[i] == '{':
                            brace_count += 1
                        elif row_str[i] == '}':
                            brace_count -= 1
                        i += 1

                    # Extract and parse the tuple
                    tuple_content = row_str[tuple_start:i]
                    parsed_tuple = self.parse_erlang_tuple(tuple_content)
                    if parsed_tuple:
                        row.append(parsed_tuple)
                else:
                    # Skip unexpected characters
                    i += 1

            return row

        except Exception as e:
            print(f"⚠️ Error parsing row '{row_str}': {e}")
            return []

    def parse_erlang_tuple(self, tuple_str):
        """Parse Erlang tuple syntax like {atom1, atom2, atom3, atom4}"""
        try:
            # Remove outer braces
            tuple_str = tuple_str.strip()
            if tuple_str.startswith('{') and tuple_str.endswith('}'):
                tuple_str = tuple_str[1:-1]

            # Split by comma, but be careful about nested structures
            parts = []
            current_part = ""
            paren_count = 0
            brace_count = 0

            for char in tuple_str:
                if char == '(':
                    paren_count += 1
                elif char == ')':
                    paren_count -= 1
                elif char == '{':
                    brace_count += 1
                elif char == '}':
                    brace_count -= 1
                elif char == ',' and paren_count == 0 and brace_count == 0:
                    # This comma is a separator
                    parts.append(current_part.strip())
                    current_part = ""
                    continue

                current_part += char

            # Don't forget the last part
            if current_part.strip():
                parts.append(current_part.strip())

            # Ensure we have exactly 4 elements
            if len(parts) == 4:
                return tuple(parts)
            else:
                print(f"⚠️ Tuple has {len(parts)} elements instead of 4: {parts}")
                return None

        except Exception as e:
            print(f"⚠️ Error parsing tuple '{tuple_str}': {e}")
            return None

    def validate_grid_structure(self, grid):
        """Validate that the grid has the correct structure"""
        try:
            if not isinstance(grid, list):
                return False

            if len(grid) != MAP_SIZE:
                print(f"⚠️ Grid has {len(grid)} rows instead of {MAP_SIZE}")
                return False

            for i, row in enumerate(grid):
                if not isinstance(row, list):
                    print(f"⚠️ Row {i} is not a list")
                    return False

                if len(row) != MAP_SIZE:
                    print(f"⚠️ Row {i} has {len(row)} columns instead of {MAP_SIZE}")
                    return False

                for j, cell in enumerate(row):
                    if not isinstance(cell, tuple) or len(cell) != 4:
                        print(f"⚠️ Cell ({i}, {j}) is not a 4-tuple: {cell}")
                        return False

            return True
        except Exception as e:
            print(f"⚠️ Error validating grid structure: {e}")
            return False

    def check_file_changes(self):
        """Check if the map file has been modified and reload if needed"""
        try:
            current_time = os.path.getmtime("test_unified_map.erl")
            if hasattr(self, 'last_file_time') and current_time > self.last_file_time:
                print("🔄 Map file changed, reloading...")
                self.reload_map_data()
            self.last_file_time = current_time
        except FileNotFoundError:
            if hasattr(self, 'last_file_time'):
                print("⚠️ Map file not found")
        except Exception as e:
            print(f"⚠️ Error checking file: {e}")

    def reload_map_data(self):
        """Reload map data and detect changes for animations"""
        # Store previous state
        self.previous_map_data = self.map_data.copy() if self.map_data else None

        # Load new data
        new_map_data = self.load_test_data()

        if self.previous_map_data and new_map_data:
            # Detect and animate changes
            self.detect_game_changes(self.previous_map_data, new_map_data)

        # Update current data
        self.map_data = new_map_data
        print("✅ Map reloaded successfully!")

    def detect_game_changes(self, old_data, new_data):
        """Detect all game state changes and trigger appropriate animations"""
        # Detect player movements
        self.detect_player_movements(old_data['player_starts'], new_data['player_starts'])

        # Detect bomb changes
        self.detect_bomb_changes(old_data, new_data)

        # Detect explosions (new unbreakable walls or destroyed breakables)
        self.detect_explosions(old_data['tiles'], new_data['tiles'])

        # Detect power-up changes
        self.detect_powerup_changes(old_data['powerups'], new_data['powerups'])

        # Detect tile changes (walls destroyed/created)
        self.detect_tile_changes(old_data['tiles'], new_data['tiles'])

    def detect_player_movements(self, old_players, new_players):
        """Detect player position changes and create walking animations"""
        old_positions = {p['player']: (p['x'], p['y']) for p in old_players}
        new_positions = {p['player']: (p['x'], p['y']) for p in new_players}

        for player_id in new_positions:
            if player_id in old_positions:
                old_pos = old_positions[player_id]
                new_pos = new_positions[player_id]

                if old_pos != new_pos:
                    # Player moved! Create walking animation
                    self.create_player_walking_animation(player_id, old_pos, new_pos)
                    print(f"🚶 Player {player_id} moved from {old_pos} to {new_pos}")

    def detect_bomb_changes(self, old_data, new_data):
        """Detect bomb placement/removal and create bomb animations"""
        # In a full implementation, this would check bomb positions from the game state
        # For now, we'll detect based on tile changes to 'bomb' type
        for x in range(MAP_SIZE):
            for y in range(MAP_SIZE):
                old_tile = old_data['tiles'][x][y]
                new_tile = new_data['tiles'][x][y]

                # If a bomb was placed (this would be a new tile type in full implementation)
                if old_tile == 0 and new_tile == 0:  # Free space
                    # Check if there's bomb info in the cell data (would need bomb_type from Erlang)
                    pass  # Placeholder for bomb detection

    def detect_explosions(self, old_tiles, new_tiles):
        """Detect explosions by finding destroyed breakable walls"""
        for x in range(MAP_SIZE):
            for y in range(MAP_SIZE):
                old_tile = old_tiles[x][y]
                new_tile = new_tiles[x][y]

                # Breakable wall destroyed = explosion happened here
                if old_tile == 1 and new_tile == 0:  # Breakable -> Free
                    self.create_explosion_animation(x, y)
                    print(f"💥 Explosion detected at ({x}, {y})")

    def detect_powerup_changes(self, old_powerups, new_powerups):
        """Detect power-up pickup and create pickup animations"""
        for x in range(MAP_SIZE):
            for y in range(MAP_SIZE):
                old_powerup = old_powerups[x][y]
                new_powerup = new_powerups[x][y]

                # Power-up was picked up
                if old_powerup != 'none' and new_powerup == 'none':
                    self.create_powerup_pickup_animation(x, y, old_powerup)
                    print(f"✨ Power-up '{old_powerup}' picked up at ({x}, {y})")

                # New power-up appeared (from destroyed breakable)
                elif old_powerup == 'none' and new_powerup != 'none':
                    self.create_powerup_spawn_animation(x, y, new_powerup)
                    print(f"🎁 Power-up '{new_powerup}' spawned at ({x}, {y})")

    def detect_tile_changes(self, old_tiles, new_tiles):
        """Detect any other tile changes"""
        for x in range(MAP_SIZE):
            for y in range(MAP_SIZE):
                old_tile = old_tiles[x][y]
                new_tile = new_tiles[x][y]

                if old_tile != new_tile:
                    self.create_tile_change_effect(x, y, old_tile, new_tile)

    def create_player_walking_animation(self, player_id, old_pos, new_pos):
        """Create smooth walking animation for player movement"""
        self.player_animations[player_id] = {
            'type': 'walking',
            'start_pos': old_pos,
            'end_pos': new_pos,
            'start_time': self.time,
            'duration': 0.5,  # 0.5 seconds walking time
            'active': True
        }

    def create_explosion_animation(self, x, y):
        """Create explosion effect animation"""
        self.explosion_animations.append({
            'x': x,
            'y': y,
            'start_time': self.time,
            'duration': 1.0,  # 1 second explosion
            'active': True
        })

        # Add camera shake
        self.camera_shake = 0.3

    def create_powerup_pickup_animation(self, x, y, powerup_type):
        """Create power-up pickup effect"""
        self.powerup_animations.append({
            'type': 'pickup',
            'x': x,
            'y': y,
            'powerup': powerup_type,
            'start_time': self.time,
            'duration': 0.8,
            'active': True
        })

    def create_powerup_spawn_animation(self, x, y, powerup_type):
        """Create power-up spawn effect"""
        self.powerup_animations.append({
            'type': 'spawn',
            'x': x,
            'y': y,
            'powerup': powerup_type,
            'start_time': self.time,
            'duration': 0.6,
            'active': True
        })

    def create_tile_change_effect(self, x, y, old_tile, new_tile):
        """Create effect for tile changes"""
        self.game_effects.append({
            'type': 'tile_change',
            'x': x,
            'y': y,
            'old_tile': old_tile,
            'new_tile': new_tile,
            'start_time': self.time,
            'duration': 0.4,
            'active': True
        })

    def update_animations(self):
        """Update all active animations"""
        current_time = self.time

        # Update player walking animations
        for player_id in list(self.player_animations.keys()):
            anim = self.player_animations[player_id]
            if anim['active']:
                elapsed = current_time - anim['start_time']
                if elapsed >= anim['duration']:
                    # Animation finished
                    del self.player_animations[player_id]

        # Update explosion animations
        self.explosion_animations = [
            anim for anim in self.explosion_animations
            if current_time - anim['start_time'] < anim['duration']
        ]

        # Update power-up animations
        self.powerup_animations = [
            anim for anim in self.powerup_animations
            if current_time - anim['start_time'] < anim['duration']
        ]

        # Update game effects
        self.game_effects = [
            effect for effect in self.game_effects
            if current_time - effect['start_time'] < effect['duration']
        ]

        # Update camera shake
        if self.camera_shake > 0:
            self.camera_shake -= 1 / FPS
        """Fallback grid data if file parsing fails"""
        # Create a simple test grid with proper 4-tuple structure
        grid = []

        for x in range(MAP_SIZE):
            row = []
            for y in range(MAP_SIZE):
                # Border walls
                if x == 0 or x == MAP_SIZE - 1 or y == 0 or y == MAP_SIZE - 1:
                    row.append(('unbreakable', 'none', 'none', 'none'))
                # Player start positions
                elif (x, y) == (1, 1):
                    row.append(('player_start', 'none', 'none', 'player_1'))
                elif (x, y) == (1, MAP_SIZE - 2):
                    row.append(('player_start', 'none', 'none', 'player_2'))
                elif (x, y) == (MAP_SIZE - 2, 1):
                    row.append(('player_start', 'none', 'none', 'player_3'))
                elif (x, y) == (MAP_SIZE - 2, MAP_SIZE - 2):
                    row.append(('player_start', 'none', 'none', 'player_4'))
                # Some random breakable walls with power-ups
                elif random.random() < 0.3 and x > 2 and y > 2 and x < MAP_SIZE - 3 and y < MAP_SIZE - 3:
                    powerups = ['none', 'none', 'none', 'move_speed', 'plus_bombs', 'bigger_explosion', 'kick_bomb']
                    powerup = random.choice(powerups)
                    row.append(('breakable', powerup, 'none', 'none'))
                # Free spaces
                else:
                    row.append(('free', 'none', 'none', 'none'))
            grid.append(row)

        print(f"✅ Generated fallback grid: {len(grid)}x{len(grid[0]) if grid else 0}")
        return grid

    def load_player_stats(self):
        """Load player statistics"""
        return {
            1: {
                'life': 3,
                'speed': 1,
                'bombs': 3,
                'explosion_radius': 2,
                'special_abilities': [],  # No special abilities yet - only bomb factory, ghost, freeze, kick
                'color': COLORS['PLAYER_1']
            },
            2: {
                'life': 2,
                'speed': 2,  # move_speed increases this
                'bombs': 4,
                'explosion_radius': 1,
                'special_abilities': ['kick_bomb'],  # Only special abilities
                'color': COLORS['PLAYER_2']
            },
            3: {
                'life': 4,
                'speed': 1,
                'bombs': 2,
                'explosion_radius': 3,  # bigger_explosion increases this
                'special_abilities': [],  # No special abilities yet
                'color': COLORS['PLAYER_3']
            },
            4: {
                'life': 1,
                'speed': 3,
                'bombs': 5,
                'explosion_radius': 1,
                'special_abilities': ['plus_bombs', 'phased', 'freeze_bomb'],  # Only special abilities
                'color': COLORS['PLAYER_4']
            }
        }

    def load_test_data(self):
        """Load map data from Erlang unified grid format"""
        print("🔄 Loading map data...")

        # Try to read from Erlang file first
        erlang_grid = self.parse_erlang_map_file("test_unified_map.erl")

        # Tile type mapping (atoms to numbers)
        tile_mapping = {
            'free': 0,
            'breakable': 1,
            'unbreakable': 2,
            'strong': 3,
            'player_start': 4
        }

        # Powerup mapping (atoms to strings)
        powerup_mapping = {
            'none': 'none',
            'move_speed': 'move_speed',
            'remote_ignition': 'remote_ignition',
            'repeat_bombs': 'repeat_bombs',
            'kick_bomb': 'kick_bomb',
            'phased': 'phased',
            'plus_bombs': 'plus_bombs',
            'bigger_explosion': 'bigger_explosion',
            'plus_life': 'plus_life',
            'freeze_bomb': 'freeze_bomb'
        }

        # Convert to the format expected by the current visualizer
        # Transpose the data during loading to match the expected coordinate system
        tiles = [[0 for _ in range(MAP_SIZE)] for _ in range(MAP_SIZE)]
        powerups = [['none' for _ in range(MAP_SIZE)] for _ in range(MAP_SIZE)]
        player_starts = []

        for row_idx in range(len(erlang_grid)):
            for col_idx in range(len(erlang_grid[row_idx])):
                cell = erlang_grid[row_idx][col_idx]
                tile_type, powerup_type, bomb_type, player_id = cell

                # Convert tile type to number
                tile_num = tile_mapping.get(tile_type, 0)
                # Transpose: store col_idx as the first index, row_idx as second
                tiles[col_idx][row_idx] = tile_num

                # Convert powerup to string
                powerup_str = powerup_mapping.get(powerup_type, 'none')
                powerups[col_idx][row_idx] = powerup_str

                # Track player starts with transposed coordinates
                if tile_type == 'player_start' and player_id != 'none':
                    player_num = int(player_id.split('_')[1])  # Extract number from 'player_1', etc.
                    # Store transposed coordinates
                    player_starts.append({'player': player_num, 'x': col_idx, 'y': row_idx})

        print(f"✅ Successfully loaded map!")
        print(f"📊 Map size: {len(tiles)}x{len(tiles[0]) if tiles else 0}")
        print(f"👥 Players found: {len(player_starts)}")

        return {
            'tiles': tiles,
            'powerups': powerups,
            'player_starts': player_starts
        }

    def draw_gradient_rect(self, surface, color1, color2, rect, vertical=True):
        """Smooth gradient rectangle"""
        if vertical:
            for y in range(rect.height):
                ratio = y / rect.height
                r = int(color1[0] * (1 - ratio) + color2[0] * ratio)
                g = int(color1[1] * (1 - ratio) + color2[1] * ratio)
                b = int(color1[2] * (1 - ratio) + color2[2] * ratio)
                pygame.draw.line(surface, (r, g, b),
                                 (rect.x, rect.y + y), (rect.x + rect.width, rect.y + y))
        else:
            for x in range(rect.width):
                ratio = x / rect.width
                r = int(color1[0] * (1 - ratio) + color2[0] * ratio)
                g = int(color1[1] * (1 - ratio) + color2[1] * ratio)
                b = int(color1[2] * (1 - ratio) + color2[2] * ratio)
                pygame.draw.line(surface, (r, g, b),
                                 (rect.x + x, rect.y), (rect.x + x, rect.y + rect.height))

    def draw_enhanced_shadow(self, surface, x, y, width, height, intensity=60):
        """Drop shadow"""
        shadow_surface = pygame.Surface((width + 8, height + 8), pygame.SRCALPHA)
        for i in range(4):
            alpha = intensity - i * 15
            if alpha > 0:
                pygame.draw.rect(shadow_surface, (0, 0, 0, alpha),
                                 (i, i, width, height))
        surface.blit(shadow_surface, (x - 2, y - 2))

    def draw_enhanced_floor(self, surface, x, y):
        """Floor tile"""
        rect = pygame.Rect(x, y, TILE_SIZE, TILE_SIZE)

        # Base gradient
        self.draw_gradient_rect(surface, COLORS['FLOOR_LIGHT'], COLORS['FLOOR_DARK'], rect)

        # Subtle texture lines
        for i in range(4):
            line_y = y + i * (TILE_SIZE // 4)
            pygame.draw.line(surface, COLORS['FLOOR_SHADOW'],
                             (x, line_y), (x + TILE_SIZE, line_y), 1)

        # Border highlight
        pygame.draw.rect(surface, COLORS['FLOOR_LIGHT'], rect, 2)
        pygame.draw.rect(surface, COLORS['FLOOR_SHADOW'], rect, 1)

    def draw_enhanced_brick_wall(self, surface, x, y):
        """Brick wall with depth"""
        # Drop shadow
        self.draw_enhanced_shadow(surface, x, y, TILE_SIZE, TILE_SIZE, 80)

        # Main wall gradient
        rect = pygame.Rect(x, y, TILE_SIZE, TILE_SIZE)
        self.draw_gradient_rect(surface, COLORS['BRICK_TOP'], COLORS['BRICK_DARK'], rect)

        # Brick pattern with realistic mortar
        brick_height = TILE_SIZE // 4
        for row in range(4):
            brick_y = y + row * brick_height

            # Mortar lines (horizontal)
            pygame.draw.line(surface, COLORS['MORTAR'],
                             (x, brick_y), (x + TILE_SIZE, brick_y), 2)

            # Brick separation (vertical, offset pattern)
            offset = (TILE_SIZE // 3) if row % 2 == 0 else 0
            for i in range(3):
                brick_x = x + offset + i * (TILE_SIZE // 3)
                if x <= brick_x < x + TILE_SIZE:
                    pygame.draw.line(surface, COLORS['MORTAR'],
                                     (brick_x, brick_y), (brick_x, brick_y + brick_height), 2)

        # Depth highlight
        pygame.draw.line(surface, COLORS['BRICK_TOP'], (x, y), (x + TILE_SIZE, y), 2)
        pygame.draw.line(surface, COLORS['BRICK_TOP'], (x, y), (x, y + TILE_SIZE), 2)

    def draw_enhanced_wooden_barrel(self, surface, x, y, has_powerup=False):
        """Wooden barrel with realistic shading"""
        # Drop shadow
        self.draw_enhanced_shadow(surface, x, y, TILE_SIZE, TILE_SIZE, 70)

        # Barrel body with curve effect
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2

        # Main barrel shape (wider in middle)
        for i in range(TILE_SIZE):
            y_pos = y + i
            # Create barrel curve
            curve_factor = 1.0 + 0.2 * math.sin((i / TILE_SIZE) * math.pi)
            width = int((TILE_SIZE - 12) * curve_factor)

            # Color gradient from top to bottom
            ratio = i / TILE_SIZE
            r = int(COLORS['WOOD_LIGHT'][0] * (1 - ratio) + COLORS['WOOD_DARK'][0] * ratio)
            g = int(COLORS['WOOD_LIGHT'][1] * (1 - ratio) + COLORS['WOOD_DARK'][1] * ratio)
            b = int(COLORS['WOOD_LIGHT'][2] * (1 - ratio) + COLORS['WOOD_DARK'][2] * ratio)

            pygame.draw.line(surface, (r, g, b),
                             (center_x - width // 2, y_pos), (center_x + width // 2, y_pos), 1)

        # Metal bands with depth
        band_positions = [0.2, 0.5, 0.8]
        for band_ratio in band_positions:
            band_y = int(y + TILE_SIZE * band_ratio)
            band_width = int((TILE_SIZE - 8) * (1.0 + 0.2 * math.sin(band_ratio * math.pi)))

            # Band shadow
            pygame.draw.rect(surface, COLORS['WOOD_SHADOW'],
                             (center_x - band_width // 2, band_y - 2, band_width, 5))
            # Band highlight
            pygame.draw.rect(surface, COLORS['WOOD_BAND'],
                             (center_x - band_width // 2, band_y - 3, band_width, 4))
            # Band shine
            pygame.draw.rect(surface, COLORS['WOOD_HIGHLIGHT'],
                             (center_x - band_width // 2, band_y - 3, band_width, 1))

        # Wood grain lines
        for i in range(6):
            grain_x = x + 8 + i * 6
            if grain_x < x + TILE_SIZE - 8:
                pygame.draw.line(surface, COLORS['WOOD_SHADOW'],
                                 (grain_x, y + 6), (grain_x, y + TILE_SIZE - 6), 1)

        # Highlight edge
        pygame.draw.line(surface, COLORS['WOOD_HIGHLIGHT'],
                         (x + 6, y + 4), (x + 6, y + TILE_SIZE - 4), 2)

        # Power-up glow effect
        if has_powerup:
            self.draw_powerup_glow(surface, center_x, center_y)

    def draw_enhanced_metal_barrel(self, surface, x, y, has_powerup=False):
        """Shiny metal barrel with reflections"""
        # Drop shadow
        self.draw_enhanced_shadow(surface, x, y, TILE_SIZE, TILE_SIZE, 70)

        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2

        # Main barrel shape with metallic gradient
        for i in range(TILE_SIZE):
            y_pos = y + i
            curve_factor = 1.0 + 0.2 * math.sin((i / TILE_SIZE) * math.pi)
            width = int((TILE_SIZE - 12) * curve_factor)

            # Metallic gradient
            ratio = i / TILE_SIZE
            r = int(COLORS['METAL_LIGHT'][0] * (1 - ratio) + COLORS['METAL_DARK'][0] * ratio)
            g = int(COLORS['METAL_LIGHT'][1] * (1 - ratio) + COLORS['METAL_DARK'][1] * ratio)
            b = int(COLORS['METAL_LIGHT'][2] * (1 - ratio) + COLORS['METAL_DARK'][2] * ratio)

            pygame.draw.line(surface, (r, g, b),
                             (center_x - width // 2, y_pos), (center_x + width // 2, y_pos), 1)

        # Metal bands
        band_positions = [0.25, 0.75]
        for band_ratio in band_positions:
            band_y = int(y + TILE_SIZE * band_ratio)
            band_width = int((TILE_SIZE - 8) * (1.0 + 0.2 * math.sin(band_ratio * math.pi)))

            pygame.draw.rect(surface, COLORS['METAL_BAND'],
                             (center_x - band_width // 2, band_y - 2, band_width, 4))
            pygame.draw.rect(surface, COLORS['METAL_SHINE'],
                             (center_x - band_width // 2, band_y - 2, band_width, 1))

        # Metallic shine/reflection
        shine_width = 4
        pygame.draw.rect(surface, COLORS['METAL_SHINE'],
                         (center_x - 8, y + 6, shine_width, TILE_SIZE - 12))

        # Secondary reflection
        pygame.draw.rect(surface, COLORS['METAL_LIGHT'],
                         (center_x + 4, y + 8, 2, TILE_SIZE - 16))

        # Power-up glow effect
        if has_powerup:
            self.draw_powerup_glow(surface, center_x, center_y)

    def draw_powerup_glow(self, surface, center_x, center_y):
        """Power-up glow effect"""
        glow_intensity = 0.7 + 0.3 * math.sin(self.powerup_pulse * 4)
        glow_size = int(20 + 8 * math.sin(self.powerup_pulse * 3))

        # Multiple glow layers for smooth effect
        for radius in range(glow_size, 0, -3):
            alpha = int(30 * glow_intensity * (radius / glow_size))
            if alpha > 0:
                glow_surf = pygame.Surface((radius * 2, radius * 2), pygame.SRCALPHA)
                pygame.draw.circle(glow_surf, (*COLORS['POWERUP_GLOW'], alpha),
                                   (radius, radius), radius)
                surface.blit(glow_surf, (center_x - radius, center_y - radius))

    def draw_enhanced_player(self, surface, x, y, player_num):
        """Animated player character"""
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2

        # Player colors
        player_colors = {
            1: COLORS['PLAYER_1'], 2: COLORS['PLAYER_2'],
            3: COLORS['PLAYER_3'], 4: COLORS['PLAYER_4']
        }
        outfit_color = player_colors.get(player_num, COLORS['PLAYER_1'])

        # Gentle bobbing animation
        bob_offset = math.sin(self.time * 4 + player_num * 1.5) * 2
        char_y = center_y + bob_offset

        # Drop shadow
        shadow_surf = pygame.Surface((TILE_SIZE, 12), pygame.SRCALPHA)
        pygame.draw.ellipse(shadow_surf, COLORS['SHADOW'], (4, 0, TILE_SIZE - 8, 12))
        surface.blit(shadow_surf, (x, y + TILE_SIZE - 8))

        # Body with gradient
        body_rect = pygame.Rect(center_x - 8, char_y - 2, 16, 20)
        self.draw_gradient_rect(surface, outfit_color,
                                tuple(max(0, c - 40) for c in outfit_color), body_rect)
        pygame.draw.rect(surface, tuple(max(0, c - 60) for c in outfit_color), body_rect, 2)

        # Head with skin gradient
        head_y = char_y - 12
        pygame.draw.circle(surface, COLORS['SKIN_SHADOW'], (center_x + 1, head_y + 1), 10)
        pygame.draw.circle(surface, COLORS['SKIN'], (center_x, head_y), 10)
        pygame.draw.circle(surface, tuple(max(0, c - 30) for c in COLORS['SKIN']),
                           (center_x, head_y), 10, 1)

        # Left eye
        pygame.draw.ellipse(surface, (255, 255, 255), (center_x - 6, head_y - 4, 6, 4))
        pygame.draw.circle(surface, (0, 0, 0), (center_x - 3, head_y - 2), 2)
        pygame.draw.circle(surface, (255, 255, 255), (center_x - 2, head_y - 3), 1)

        # Right eye
        pygame.draw.ellipse(surface, (255, 255, 255), (center_x + 1, head_y - 4, 6, 4))
        pygame.draw.circle(surface, (0, 0, 0), (center_x + 4, head_y - 2), 2)
        pygame.draw.circle(surface, (255, 255, 255), (center_x + 5, head_y - 3), 1)

        # Eyebrows
        pygame.draw.arc(surface, (101, 67, 33), (center_x - 6, head_y - 7, 5, 4), 0, math.pi, 2)
        pygame.draw.arc(surface, (101, 67, 33), (center_x + 2, head_y - 7, 5, 4), 0, math.pi, 2)

        # Nose (small dot)
        pygame.draw.circle(surface, (200, 150, 120), (center_x, head_y), 1)

        # Mouth (happy smile)
        pygame.draw.arc(surface, (150, 100, 80), (center_x - 3, head_y + 1, 6, 4), 0, math.pi, 2)

        # Arms with animation
        arm_swing = math.sin(self.time * 6 + player_num) * 3

        # Left arm
        arm_end_x = center_x - 12 + arm_swing
        arm_end_y = char_y + 6
        pygame.draw.line(surface, outfit_color, (center_x - 6, char_y + 4),
                         (arm_end_x, arm_end_y), 4)
        pygame.draw.circle(surface, COLORS['SKIN'], (arm_end_x, arm_end_y), 3)

        # Right arm
        arm_end_x = center_x + 12 - arm_swing
        pygame.draw.line(surface, outfit_color, (center_x + 6, char_y + 4),
                         (arm_end_x, arm_end_y), 4)
        pygame.draw.circle(surface, COLORS['SKIN'], (arm_end_x, arm_end_y), 3)

        # Legs with walking animation
        leg_offset = math.sin(self.time * 8 + player_num) * 2

        # Left leg
        pygame.draw.rect(surface, outfit_color,
                         (center_x - 6 + leg_offset, char_y + 16, 4, 10))
        pygame.draw.rect(surface, (40, 40, 40),
                         (center_x - 7 + leg_offset, char_y + 24, 6, 4))

        # Right leg
        pygame.draw.rect(surface, outfit_color,
                         (center_x + 2 - leg_offset, char_y + 16, 4, 10))
        pygame.draw.rect(surface, (40, 40, 40),
                         (center_x + 1 - leg_offset, char_y + 24, 6, 4))

        # Player number badge with glow
        badge_surf = pygame.Surface((16, 10), pygame.SRCALPHA)
        pygame.draw.rect(badge_surf, (255, 255, 255, 200), (0, 0, 16, 10))
        pygame.draw.rect(badge_surf, (0, 0, 0), (0, 0, 16, 10), 1)

        num_text = self.small_font.render(str(player_num), True, (0, 0, 0))
        badge_surf.blit(num_text, (5, -1))

        # Badge glow
        glow_surf = pygame.Surface((20, 14), pygame.SRCALPHA)
        pygame.draw.rect(glow_surf, (*COLORS['POWERUP_CORE'], 100), (0, 0, 20, 14))
        surface.blit(glow_surf, (center_x - 10, char_y + 20))
        surface.blit(badge_surf, (center_x - 8, char_y + 22))

    def draw_custom_icon(self, surface, icon_type, center_x, center_y, size, color):
        """Icons for power-ups"""
        if icon_type == "lightning":
            # Lightning bolt
            points = [
                (center_x - size // 3, center_y - size // 2),
                (center_x + size // 6, center_y - size // 4),
                (center_x - size // 6, center_y),
                (center_x + size // 3, center_y + size // 2),
                (center_x - size // 6, center_y + size // 4),
                (center_x + size // 6, center_y)
            ]
            pygame.draw.polygon(surface, color, points)

        elif icon_type == "remote":
            # Remote control
            pygame.draw.rect(surface, color, (center_x - size // 3, center_y - size // 2, size // 1.5, size))
            pygame.draw.circle(surface, (255, 100, 100), (center_x - size // 6, center_y - size // 4), 3)
            pygame.draw.circle(surface, (100, 255, 100), (center_x + size // 6, center_y - size // 4), 3)
            pygame.draw.line(surface, color, (center_x, center_y - size // 2), (center_x, center_y - size), 2)

        elif icon_type == "factory":
            # Big bomb with smaller bomb beside it (bomb factory)
            # Main big bomb
            pygame.draw.circle(surface, color, (center_x - size // 6, center_y), size // 2)
            pygame.draw.line(surface, color, (center_x - size // 6, center_y - size // 2),
                             (center_x - size // 3, center_y - size), 2)
            pygame.draw.circle(surface, (255, 200, 0), (center_x - size // 3, center_y - size), 2)

            # Smaller bomb beside it
            pygame.draw.circle(surface, color, (center_x + size // 4, center_y + size // 6), size // 4)
            pygame.draw.line(surface, color, (center_x + size // 4, center_y + size // 6 - size // 4),
                             (center_x + size // 6, center_y - size // 3), 1)
            pygame.draw.circle(surface, (255, 200, 0), (center_x + size // 6, center_y - size // 3), 1)

        elif icon_type == "boot":
            # Leg in side view kicking
            # Thigh
            pygame.draw.line(surface, color, (center_x - size // 3, center_y - size // 4),
                             (center_x, center_y + size // 4), 8)
            # Shin
            pygame.draw.line(surface, color, (center_x, center_y + size // 4),
                             (center_x + size // 2, center_y + size // 6), 6)
            # Foot/boot
            pygame.draw.ellipse(surface, color, (center_x + size // 3, center_y, size // 1.5, size // 3))
            # Motion lines
            for i in range(3):
                line_x = center_x + size // 2 + i * 6
                pygame.draw.line(surface, (*color[:3], 150), (line_x, center_y + size // 8),
                                 (line_x + 8, center_y + size // 8), 2)

        elif icon_type == "ghost":
            # Ghost shape
            pygame.draw.circle(surface, color, (center_x, center_y - size // 4), size // 2)
            pygame.draw.rect(surface, color, (center_x - size // 2, center_y - size // 4, size, size // 2))
            # Wavy bottom
            wave_points = []
            for i in range(5):
                x = center_x - size // 2 + i * (size // 4)
                y = center_y + size // 4 + (5 if i % 2 == 0 else -5)
                wave_points.append((x, y))
            if len(wave_points) > 2:
                pygame.draw.polygon(surface, color, [(center_x - size // 2, center_y + size // 4)] + wave_points + [
                    (center_x + size // 2, center_y + size // 4)])

        elif icon_type == "bomb":
            # Bomb
            pygame.draw.circle(surface, color, (center_x, center_y), size // 2)
            pygame.draw.line(surface, color, (center_x, center_y - size // 2), (center_x - size // 4, center_y - size),
                             3)
            pygame.draw.circle(surface, (255, 200, 0), (center_x - size // 4, center_y - size), 3)

        elif icon_type == "explosion":
            # Explosion burst
            for i in range(8):
                angle = i * math.pi / 4
                x = center_x + int(size // 2 * math.cos(angle))
                y = center_y + int(size // 2 * math.sin(angle))
                pygame.draw.line(surface, color, (center_x, center_y), (x, y), 3)
                pygame.draw.circle(surface, color, (x, y), 3)
            pygame.draw.circle(surface, color, (center_x, center_y), size // 4)

        elif icon_type == "heart":
            # Heart shape
            pygame.draw.circle(surface, color, (center_x - size // 4, center_y - size // 6), size // 3)
            pygame.draw.circle(surface, color, (center_x + size // 4, center_y - size // 6), size // 3)
            points = [
                (center_x - size // 2, center_y),
                (center_x + size // 2, center_y),
                (center_x, center_y + size // 2)
            ]
            pygame.draw.polygon(surface, color, points)

        elif icon_type == "freeze":
            # Snowflake/ice crystal
            pygame.draw.line(surface, color, (center_x, center_y - size // 2), (center_x, center_y + size // 2), 3)
            pygame.draw.line(surface, color, (center_x - size // 2, center_y), (center_x + size // 2, center_y), 3)
            pygame.draw.line(surface, color, (center_x - size // 3, center_y - size // 3),
                             (center_x + size // 3, center_y + size // 3), 3)
            pygame.draw.line(surface, color, (center_x - size // 3, center_y + size // 3),
                             (center_x + size // 3, center_y - size // 3), 3)

    def update_animations(self):
        """Update all active animations"""
        current_time = self.time

        # Update player walking animations
        for player_id in list(self.player_animations.keys()):
            anim = self.player_animations[player_id]
            if anim['active']:
                elapsed = current_time - anim['start_time']
                if elapsed >= anim['duration']:
                    # Animation finished
                    del self.player_animations[player_id]

        # Update explosion animations
        self.explosion_animations = [
            anim for anim in self.explosion_animations
            if current_time - anim['start_time'] < anim['duration']
        ]

        # Update power-up animations
        self.powerup_animations = [
            anim for anim in self.powerup_animations
            if current_time - anim['start_time'] < anim['duration']
        ]

        # Update game effects
        self.game_effects = [
            effect for effect in self.game_effects
            if current_time - effect['start_time'] < effect['duration']
        ]

        # Update camera shake
        if self.camera_shake > 0:
            self.camera_shake -= 1 / FPS

    def draw_explosion_effect(self, surface, x, y, elapsed_time, duration):
        """Draw animated explosion effect"""
        progress = elapsed_time / duration
        center_x = y * TILE_SIZE + TILE_SIZE // 2
        center_y = x * TILE_SIZE + TILE_SIZE // 2

        if progress < 0.3:  # Expanding blast
            radius = int(progress * 100)
            colors = [(255, 255, 100), (255, 150, 50), (255, 50, 50)]

            for i, color in enumerate(colors):
                explosion_radius = max(1, radius - i * 8)
                if explosion_radius > 0:
                    explosion_surf = pygame.Surface((explosion_radius * 2, explosion_radius * 2), pygame.SRCALPHA)
                    alpha = int(255 * (1 - progress * 2))
                    pygame.draw.circle(explosion_surf, (*color, alpha), (explosion_radius, explosion_radius),
                                       explosion_radius)
                    surface.blit(explosion_surf, (center_x - explosion_radius, center_y - explosion_radius))

        elif progress < 0.8:  # Fire particles
            for _ in range(8):
                angle = random.random() * 2 * math.pi
                distance = random.random() * 40
                particle_x = center_x + int(math.cos(angle) * distance)
                particle_y = center_y + int(math.sin(angle) * distance)
                particle_size = random.randint(2, 6)

                color = (255, random.randint(100, 255), random.randint(0, 100))
                pygame.draw.circle(surface, color, (particle_x, particle_y), particle_size)

    def draw_powerup_pickup_effect(self, surface, x, y, elapsed_time, duration, powerup_type):
        """Draw power-up pickup animation"""
        progress = elapsed_time / duration
        center_x = y * TILE_SIZE + TILE_SIZE // 2
        center_y = x * TILE_SIZE + TILE_SIZE // 2

        # Rising sparkles
        for i in range(6):
            sparkle_y = center_y - int(progress * 60) - i * 10
            sparkle_x = center_x + int(math.sin(self.time * 5 + i) * 15)

            if sparkle_y > center_y - 70:
                alpha = int(255 * (1 - progress))
                color = (*COLORS['POWERUP_CORE'][:3], alpha)
                sparkle_surf = pygame.Surface((8, 8), pygame.SRCALPHA)
                pygame.draw.circle(sparkle_surf, color, (4, 4), 4)
                surface.blit(sparkle_surf, (sparkle_x - 4, sparkle_y - 4))

    def draw_animated_player(self, surface, x, y, player_num):
        """Draw player with walking animation if active"""
        # Check if player has active walking animation
        if player_num in self.player_animations:
            anim = self.player_animations[player_num]
            elapsed = self.time - anim['start_time']
            progress = min(elapsed / anim['duration'], 1.0)

            # Interpolate position
            start_x, start_y = anim['start_pos']
            end_x, end_y = anim['end_pos']

            current_x = start_x + (end_x - start_x) * progress
            current_y = start_y + (end_y - start_y) * progress

            # Calculate screen position
            screen_x = current_y * TILE_SIZE
            screen_y = current_x * TILE_SIZE

            # Enhanced walking animation
            walk_bounce = math.sin(progress * math.pi * 4) * 3
            self.draw_enhanced_player(surface, screen_x, screen_y - walk_bounce, player_num)
        else:
            # Normal static player
            self.draw_enhanced_player(surface, x, y, player_num)

    def draw_enhanced_map(self):
        """Draw the complete enhanced map with all animations"""
        # Apply camera shake
        shake_x = int(random.random() * self.camera_shake * 10) if self.camera_shake > 0 else 0
        shake_y = int(random.random() * self.camera_shake * 10) if self.camera_shake > 0 else 0

        self.map_surface.fill(COLORS['BACKGROUND'])

        # Update animations
        self.time += 1 / FPS
        self.powerup_pulse += 1 / FPS
        self.update_animations()

        # Check for file changes periodically
        current_time = time.time()
        if current_time - self.last_reload_check > self.reload_check_interval:
            self.check_file_changes()
            self.last_reload_check = current_time

        # Draw tiles with shake offset
        for x in range(MAP_SIZE):
            for y in range(MAP_SIZE):
                pixel_x = y * TILE_SIZE + shake_x
                pixel_y = x * TILE_SIZE + shake_y

                tile_type = self.map_data['tiles'][x][y]
                powerup = self.map_data['powerups'][x][y]
                has_powerup = powerup != "none"

                # Draw floor
                if tile_type != 2:
                    self.draw_enhanced_floor(self.map_surface, pixel_x, pixel_y)

                # Draw objects
                if tile_type == 1:  # BREAKABLE
                    self.draw_enhanced_wooden_barrel(self.map_surface, pixel_x, pixel_y, has_powerup)
                elif tile_type == 2:  # UNBREAKABLE
                    self.draw_enhanced_brick_wall(self.map_surface, pixel_x, pixel_y)
                elif tile_type == 3:  # STRONG
                    self.draw_enhanced_metal_barrel(self.map_surface, pixel_x, pixel_y, has_powerup)
                elif tile_type == 4:  # PLAYER_START
                    # Find and draw player with animation
                    for player in self.map_data['player_starts']:
                        if player['x'] == x and player['y'] == y:
                            self.draw_animated_player(self.map_surface, pixel_x, pixel_y, player['player'])
                            break

                # Selection highlight
                if self.selected_tile == (x, y):
                    highlight_surf = pygame.Surface((TILE_SIZE, TILE_SIZE), pygame.SRCALPHA)
                    pygame.draw.rect(highlight_surf, COLORS['SELECTION'], (0, 0, TILE_SIZE, TILE_SIZE))
                    self.map_surface.blit(highlight_surf, (pixel_x, pixel_y))

        # Draw all animation effects on top
        self.draw_all_effects()

        # Blit map to virtual surface at the right position
        self.virtual_surface.blit(self.map_surface, (MAP_OFFSET_X, MAP_OFFSET_Y))

    def draw_all_effects(self):
        """Draw all active animation effects"""
        current_time = self.time

        # Draw explosions
        for explosion in self.explosion_animations:
            elapsed = current_time - explosion['start_time']
            self.draw_explosion_effect(self.map_surface, explosion['x'], explosion['y'],
                                       elapsed, explosion['duration'])

        # Draw power-up effects
        for powerup_anim in self.powerup_animations:
            elapsed = current_time - powerup_anim['start_time']
            if powerup_anim['type'] == 'pickup':
                self.draw_powerup_pickup_effect(self.map_surface, powerup_anim['x'], powerup_anim['y'],
                                                elapsed, powerup_anim['duration'], powerup_anim['powerup'])

        # Draw tile change effects
        for effect in self.game_effects:
            elapsed = current_time - effect['start_time']
            if effect['type'] == 'tile_change':
                self.draw_tile_change_effect(effect['x'], effect['y'], elapsed, effect['duration'])

    def draw_tile_change_effect(self, x, y, elapsed_time, duration):
        """Draw tile change effect"""
        progress = elapsed_time / duration
        center_x = y * TILE_SIZE + TILE_SIZE // 2
        center_y = x * TILE_SIZE + TILE_SIZE // 2

        # Pulsing circle effect
        radius = int(20 * (1 - progress))
        alpha = int(100 * (1 - progress))

        if radius > 0:
            effect_surf = pygame.Surface((radius * 2, radius * 2), pygame.SRCALPHA)
            pygame.draw.circle(effect_surf, (255, 255, 255, alpha), (radius, radius), radius, 3)
            self.map_surface.blit(effect_surf, (center_x - radius, center_y - radius))

    def draw_player_stats_panel(self):
        """Draw player statistics panel on the left side"""
        self.player_panel_surface.fill(COLORS['UI_BACKGROUND'])

        # Panel title
        title_text = "PLAYERS"
        title_surface = self.title_font.render(title_text, True, COLORS['TEXT_GOLD'])
        title_shadow = self.title_font.render(title_text, True, COLORS['TEXT_SHADOW'])

        self.player_panel_surface.blit(title_shadow, (12, 12))
        self.player_panel_surface.blit(title_surface, (10, 10))

        # Draw each player's stats
        start_y = 60
        player_height = (MAP_SIZE * TILE_SIZE - 80) // 4  # Divide remaining space by 4 players

        for player_id in range(1, 5):
            y_pos = start_y + (player_id - 1) * player_height
            self.draw_single_player_stats(self.player_panel_surface, player_id, y_pos, player_height)

        # Blit to virtual surface
        self.virtual_surface.blit(self.player_panel_surface, (0, MAP_OFFSET_Y))

    def draw_single_player_stats(self, surface, player_id, y_pos, height):
        """Draw individual player statistics with visual elements - improved layout"""
        stats = self.player_stats[player_id]
        player_color = stats['color']

        # Player background with gradient
        bg_rect = pygame.Rect(10, y_pos, PLAYER_PANEL_WIDTH - 20, height - 10)
        bg_alpha = int(40 + 20 * math.sin(self.time * 2 + player_id))
        bg_surf = pygame.Surface((bg_rect.width, bg_rect.height), pygame.SRCALPHA)

        # Create gradient background
        for y in range(bg_rect.height):
            ratio = y / bg_rect.height
            alpha = int(bg_alpha * (1 - ratio * 0.5))
            color = (*player_color[:3], alpha)
            pygame.draw.line(bg_surf, color, (0, y), (bg_rect.width, y))

        surface.blit(bg_surf, (bg_rect.x, bg_rect.y))

        # Player avatar (mini player drawing)
        avatar_x = 25
        avatar_y = y_pos + 10
        self.draw_mini_player(surface, avatar_x, avatar_y, player_id, scale=0.6)

        # Player ID
        player_text = f"PLAYER {player_id}"
        player_surface = self.small_font.render(player_text, True, COLORS['TEXT_WHITE'])
        surface.blit(player_surface, (avatar_x - 15, avatar_y + 25))

        # Statistics with visual elements
        stats_start_y = y_pos + 10
        stat_height = 18

        # Life - Draw hearts
        life_text = "Life:"
        life_surface = self.small_font.render(life_text, True, (255, 100, 120))
        surface.blit(life_surface, (80, stats_start_y))

        # Draw hearts for life count
        heart_start_x = 115
        for i in range(stats['life']):
            heart_x = heart_start_x + i * 12
            self.draw_mini_heart(surface, heart_x, stats_start_y + 4, (255, 100, 120))

        # Speed
        speed_text = f"Speed: {stats['speed']}"
        speed_surface = self.small_font.render(speed_text, True, COLORS['TEXT_CYAN'])
        surface.blit(speed_surface, (80, stats_start_y + stat_height))

        # Bombs - Draw bomb icons
        bombs_text = "Bombs:"
        bombs_surface = self.small_font.render(bombs_text, True, (255, 150, 100))
        surface.blit(bombs_surface, (80, stats_start_y + stat_height * 2))

        # Draw bombs for bomb count
        bomb_start_x = 125
        for i in range(stats['bombs']):
            bomb_x = bomb_start_x + i * 12
            self.draw_mini_bomb(surface, bomb_x, stats_start_y + stat_height * 2 + 4, (255, 150, 100))

        # Explosion radius
        radius_text = f"Radius: {stats['explosion_radius']}"
        radius_surface = self.small_font.render(radius_text, True, (255, 200, 0))
        surface.blit(radius_surface, (80, stats_start_y + stat_height * 3))

        # Special abilities with actual powerup icons (only special abilities, not stat boosts)
        abilities_text = "Abilities:"
        abilities_surface = self.small_font.render(abilities_text, True, COLORS['TEXT_WHITE'])
        surface.blit(abilities_surface, (80, stats_start_y + stat_height * 4))

        # Filter to only show special abilities (not basic stat boosts)
        special_only_abilities = [ability for ability in stats['special_abilities']
                                  if ability in ['plus_bombs', 'repeat_bombs', 'phased', 'freeze_bomb', 'kick_bomb']]

        # Draw ability icons with proper powerup icons
        ability_x = 85
        ability_y = stats_start_y + stat_height * 5 + 8
        for i, ability in enumerate(special_only_abilities[:3]):  # Max 3 abilities shown
            icon_x = ability_x + i * 20  # Slightly closer spacing
            # Make sure icons stay within the player's area
            if icon_x + 12 < PLAYER_PANEL_WIDTH - 20:
                self.draw_ability_powerup_icon(surface, icon_x, ability_y, ability)

    def draw_mini_player(self, surface, x, y, player_num, scale=1.0):
        """Draw a mini version of the player character"""
        player_colors = {
            1: COLORS['PLAYER_1'], 2: COLORS['PLAYER_2'],
            3: COLORS['PLAYER_3'], 4: COLORS['PLAYER_4']
        }
        outfit_color = player_colors.get(player_num, COLORS['PLAYER_1'])

        # Scale dimensions
        size = int(16 * scale)

        # Body
        body_rect = pygame.Rect(x - size // 2, y, size, int(size * 1.2))
        pygame.draw.rect(surface, outfit_color, body_rect)
        pygame.draw.rect(surface, tuple(max(0, c - 40) for c in outfit_color), body_rect, 1)

        # Head
        head_y = y - size // 2
        pygame.draw.circle(surface, COLORS['SKIN'], (x, head_y), size // 2)
        pygame.draw.circle(surface, tuple(max(0, c - 30) for c in COLORS['SKIN']), (x, head_y), size // 2, 1)

        # Simple face
        # Eyes
        pygame.draw.circle(surface, (0, 0, 0), (x - size // 4, head_y - 2), 1)
        pygame.draw.circle(surface, (0, 0, 0), (x + size // 4, head_y - 2), 1)

        # Player number badge
        badge_surf = pygame.Surface((12, 8), pygame.SRCALPHA)
        pygame.draw.rect(badge_surf, (255, 255, 255, 200), (0, 0, 12, 8))
        pygame.draw.rect(badge_surf, (0, 0, 0), (0, 0, 12, 8), 1)

        num_text = self.small_font.render(str(player_num), True, (0, 0, 0))
        badge_surf.blit(num_text, (3, -2))
        surface.blit(badge_surf, (x - 6, y + int(size * 1.2) + 2))

    def draw_mini_heart(self, surface, x, y, color):
        """Draw a small heart icon"""
        size = 6
        # Heart shape - two circles and triangle
        pygame.draw.circle(surface, color, (x - 2, y - 1), 2)
        pygame.draw.circle(surface, color, (x + 2, y - 1), 2)
        points = [(x - 3, y), (x + 3, y), (x, y + 4)]
        pygame.draw.polygon(surface, color, points)

    def draw_mini_bomb(self, surface, x, y, color):
        """Draw a small bomb icon"""
        # Bomb body
        pygame.draw.circle(surface, color, (x, y + 2), 3)
        # Fuse
        pygame.draw.line(surface, color, (x, y - 1), (x - 2, y - 3), 1)
        # Spark
        pygame.draw.circle(surface, (255, 200, 0), (x - 2, y - 3), 1)

    def draw_ability_powerup_icon(self, surface, x, y, ability):
        """Draw proper powerup icons for abilities"""
        size = 12

        # Map abilities to icon types and colors
        ability_icon_map = {
            'kick_bomb': ('boot', (255, 100, 255)),
            'plus_bombs': ('factory', (255, 150, 100)),  # Uses our new factory icon
            'phased': ('ghost', (200, 200, 255)),
            'freeze_bomb': ('freeze', (150, 200, 255)),
            'repeat_bombs': ('factory', COLORS['TEXT_GOLD']),  # Also uses factory icon
        }

        if ability in ability_icon_map:
            icon_type, color = ability_icon_map[ability]
            icon_surf = pygame.Surface((size * 2, size * 2), pygame.SRCALPHA)
            self.draw_custom_icon(icon_surf, icon_type, size, size, size, color)
            surface.blit(icon_surf, (x - size, y - size))

    def draw_powerups_panel(self):
        """Draw power-ups panel below the map"""
        self.powerup_panel_surface.fill(COLORS['UI_BACKGROUND'])

        # Title
        title_y = 15
        title_text = "POWER-UPS"
        title_shadow = self.title_font.render(title_text, True, COLORS['TEXT_SHADOW'])
        title_main = self.title_font.render(title_text, True, COLORS['TEXT_GOLD'])

        self.powerup_panel_surface.blit(title_shadow, (22, title_y + 2))
        self.powerup_panel_surface.blit(title_main, (20, title_y))

        # Power-ups in horizontal layout
        start_x = 30
        start_y = 50
        powerup_data = [
            ("lightning", "LIGHTNING SPEED", "move_speed", COLORS['TEXT_CYAN']),
            ("remote", "REMOTE DETONATOR", "remote_ignition", COLORS['TEXT_ORANGE']),
            ("factory", "BOMB FACTORY", "repeat_bombs", COLORS['TEXT_GOLD']),
            ("boot", "POWER KICK", "kick_bomb", (255, 100, 255)),
            ("ghost", "GHOST MODE", "phased", (200, 200, 255)),
            ("bomb", "BOMB ARSENAL", "plus_bombs", (255, 150, 100)),
            ("explosion", "MEGA BLAST", "bigger_explosion", (255, 100, 100)),
            ("heart", "EXTRA LIFE", "plus_life", (255, 100, 150)),
            ("freeze", "FREEZE BOMB", "freeze_bomb", (150, 200, 255))
        ]

        items_per_row = 3
        item_width = (WINDOW_WIDTH - 60) // items_per_row
        row_height = 25  # Height per row

        for i, (icon_type, name, powerup_type, color) in enumerate(powerup_data):
            row = i // items_per_row
            col = i % items_per_row

            x_pos = start_x + col * item_width
            y_pos = start_y + row * row_height

            if y_pos < POWERUP_PANEL_HEIGHT - 25:
                # Animated background
                bg_alpha = int(20 + 15 * math.sin(self.time * 2 + i * 0.3))
                bg_width = item_width - 10
                bg_surf = pygame.Surface((bg_width, 20), pygame.SRCALPHA)

                for y in range(20):
                    ratio = y / 20
                    alpha = int(bg_alpha * (1 - ratio * 0.5))
                    bg_color = (*color[:3], alpha)
                    pygame.draw.line(bg_surf, bg_color, (0, y), (bg_width, y))

                self.powerup_panel_surface.blit(bg_surf, (x_pos - 5, y_pos - 5))

                # Icon
                icon_scale = 0.8 + 0.1 * math.sin(self.time * 3 + i)
                icon_size = int(12 * icon_scale)

                icon_surf = pygame.Surface((24, 24), pygame.SRCALPHA)
                self.draw_custom_icon(icon_surf, icon_type, 12, 12, icon_size, color)
                self.powerup_panel_surface.blit(icon_surf, (x_pos, y_pos - 2))

                # Name
                name_text = self.small_font.render(name, True, color)
                self.powerup_panel_surface.blit(name_text, (x_pos + 30, y_pos))

        # Blit to virtual surface
        self.virtual_surface.blit(self.powerup_panel_surface, (0, POWERUP_OFFSET_Y))

    def handle_events(self):
        """Enhanced event handling with new layout and resizing"""
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return False
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_ESCAPE:
                    return False
            elif event.type == pygame.VIDEORESIZE:
                self.handle_window_resize(event.w, event.h)
            elif event.type == pygame.MOUSEBUTTONDOWN:
                if event.button == 1:  # Left click
                    mouse_x, mouse_y = pygame.mouse.get_pos()

                    # Calculate the offset of the scaled content on screen
                    scaled_width = int(WINDOW_WIDTH * self.scale_factor)
                    scaled_height = int(WINDOW_HEIGHT * self.scale_factor)
                    x_offset = (self.current_width - scaled_width) // 2
                    y_offset = (self.current_height - scaled_height) // 2

                    # Convert screen coordinates to virtual coordinates
                    virtual_x = (mouse_x - max(0, x_offset)) / self.scale_factor
                    virtual_y = (mouse_y - max(0, y_offset)) / self.scale_factor

                    # Check if click is within map area (accounting for new position)
                    if (MAP_OFFSET_X <= virtual_x < MAP_OFFSET_X + MAP_SIZE * TILE_SIZE and
                            MAP_OFFSET_Y <= virtual_y < MAP_OFFSET_Y + MAP_SIZE * TILE_SIZE):

                        # Adjust mouse coordinates relative to map
                        map_mouse_x = virtual_x - MAP_OFFSET_X
                        map_mouse_y = virtual_y - MAP_OFFSET_Y

                        tile_x = map_mouse_y // TILE_SIZE
                        tile_y = map_mouse_x // TILE_SIZE

                        if 0 <= tile_x < MAP_SIZE and 0 <= tile_y < MAP_SIZE:
                            self.selected_tile = (tile_x, tile_y)
                            tile_type = self.map_data['tiles'][tile_x][tile_y]
                            powerup = self.map_data['powerups'][tile_x][tile_y]

                            tile_names = {0: 'FREE', 1: 'WOOD_BARREL', 2: 'BRICK_WALL', 3: 'METAL_BARREL',
                                          4: 'PLAYER_START'}
                            tile_name = tile_names.get(tile_type, 'UNKNOWN')

                            print(f"🎯 Enhanced Click: ({tile_x}, {tile_y}) = {tile_name} + {powerup}")

        return True

    def run(self):
        """main game loop with scaling support"""
        print("🎮 Playing with Fire 2 Map Visualizer Started!")
        print("🖱️ Click tiles to inspect | ESC to exit | Resize window as needed")

        running = True
        while running:
            running = self.handle_events()

            # Clear virtual surface with gradient background
            bg_rect = pygame.Rect(0, 0, WINDOW_WIDTH, WINDOW_HEIGHT)
            self.draw_gradient_rect(self.virtual_surface, COLORS['BACKGROUND'], COLORS['PANEL_BG'], bg_rect)

            # Draw enhanced map and UI to virtual surface
            self.draw_enhanced_map()  # Map on the right
            self.draw_player_stats_panel()  # Player stats on the left
            self.draw_powerups_panel()  # Power-ups below the map

            # Scale virtual surface to actual screen
            if self.scale_factor != 1.0:
                scaled_width = int(WINDOW_WIDTH * self.scale_factor)
                scaled_height = int(WINDOW_HEIGHT * self.scale_factor)
                scaled_surface = pygame.transform.scale(self.virtual_surface, (scaled_width, scaled_height))
            else:
                scaled_surface = self.virtual_surface

            # Clear screen and center the scaled content
            self.screen.fill(COLORS['BACKGROUND'])

            # Center the content on screen
            x_offset = (self.current_width - scaled_surface.get_width()) // 2
            y_offset = (self.current_height - scaled_surface.get_height()) // 2
            self.screen.blit(scaled_surface, (max(0, x_offset), max(0, y_offset)))

            pygame.display.flip()
            self.clock.tick(FPS)

        pygame.quit()
        sys.exit()


if __name__ == "__main__":
    visualizer = EnhancedMapVisualizer()
    visualizer.run()