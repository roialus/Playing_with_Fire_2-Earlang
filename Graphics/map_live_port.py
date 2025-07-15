import pygame
import sys
import math
import random
import os
import time
import struct
import select

# Initialize Pygame
pygame.init()

TILE_SIZE = 40  # Size of each tile in pixels
MAP_SIZE = 16  # Size of the map in tiles (16x16)
PLAYER_PANEL_WIDTH = 220  # Left panel for player stats
POWERUP_PANEL_HEIGHT = 140  # Bottom panel for power-ups
WINDOW_WIDTH = PLAYER_PANEL_WIDTH + MAP_SIZE * TILE_SIZE + 20  # Player panel + map + margin
WINDOW_HEIGHT = MAP_SIZE * TILE_SIZE + POWERUP_PANEL_HEIGHT + 20  # Map + power-ups + margin
MIN_WINDOW_WIDTH = 800  # Minimum width for the window
MIN_WINDOW_HEIGHT = 600  # Minimum height for the window
FPS = 60  # Frames per second for the game loop

# Layout offsets
MAP_OFFSET_X = PLAYER_PANEL_WIDTH + 10  # Map starts after player panel
MAP_OFFSET_Y = 10  # Small top margin
POWERUP_OFFSET_Y = MAP_OFFSET_Y + MAP_SIZE * TILE_SIZE + 10  # Power-ups below map

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

    # Bomb and explosion colors
    'BOMB_BLACK': (40, 40, 40),
    'BOMB_FUSE': (255, 100, 0),
    'EXPLOSION_CORE': (255, 255, 200),
    'EXPLOSION_MIDDLE': (255, 150, 50),
    'EXPLOSION_OUTER': (255, 50, 50),
}


class CompleteGameVisualizer:
    def __init__(self):
        # Make window resizable and start with smaller size
        initial_width = min(WINDOW_WIDTH, 900)
        initial_height = min(WINDOW_HEIGHT, 700)
        self.screen = pygame.display.set_mode((initial_width, initial_height), pygame.RESIZABLE)
        pygame.display.set_caption("🎮 Playing with Fire 2 - Live Game Viewer")
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

        # Mapping dictionaries
        self.tile_mapping = {
            'free': 0, 'breakable': 1, 'unbreakable': 2, 'strong': 3, 'player_start': 4
        }
        self.powerup_mapping = {
            'none': 'none', 'move_speed': 'move_speed', 'remote_ignition': 'remote_ignition',
            'repeat_bombs': 'repeat_bombs', 'kick_bomb': 'kick_bomb', 'phased': 'phased',
            'plus_bombs': 'plus_bombs', 'bigger_explosion': 'bigger_explosion',
            'plus_life': 'plus_life', 'freeze_bomb': 'freeze_bomb'
        }

        # Port communication setup
        self.port_buffer = b''
        self.map_initialized = False
        self.waiting_for_initial_map = True

        # Complete game state tracking for animations
        self.previous_game_state = None
        self.current_game_state = self.get_fallback_game_state()

        # Animation systems
        self.player_animations = {}  # Player movement animations
        self.bomb_animations = {}  # Bomb countdown and placement animations
        self.explosion_animations = []  # Active explosion effects
        self.powerup_animations = []  # Power-up pickup/spawn effects
        self.game_effects = []  # General effect animations (damage, status, etc.)
        self.status_effects = {}  # Player status effects (stun, invincibility, etc.)

        # Load initial game state
        self.player_stats = self.load_player_stats()

        # Create surfaces for smooth rendering
        self.map_surface = pygame.Surface((MAP_SIZE * TILE_SIZE, MAP_SIZE * TILE_SIZE))
        self.player_panel_surface = pygame.Surface((PLAYER_PANEL_WIDTH, MAP_SIZE * TILE_SIZE))
        self.powerup_panel_surface = pygame.Surface((WINDOW_WIDTH, POWERUP_PANEL_HEIGHT))

        # Virtual surface for full layout
        self.virtual_surface = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT))

        print("🎮 Game Visualizer initialized")
        print("⏳ Waiting for initial map from map_generator...")

    def read_port_data(self):
        """Read data from Erlang port using stdin"""
        try:
            # Check if data is available on stdin
            ready, _, _ = select.select([sys.stdin], [], [], 0)
            if not ready:
                return None

            # Read available data
            data = sys.stdin.buffer.read()
            if not data:
                return None

            self.port_buffer += data

            # Process complete packets (4-byte length prefix + data)
            packets = []
            while len(self.port_buffer) >= 4:
                # Read packet length (big-endian 32-bit)
                packet_length = struct.unpack('>I', self.port_buffer[:4])[0]

                if len(self.port_buffer) >= 4 + packet_length:
                    # Complete packet available
                    packet_data = self.port_buffer[4:4 + packet_length]
                    self.port_buffer = self.port_buffer[4 + packet_length:]
                    packets.append(packet_data)
                else:
                    # Incomplete packet, wait for more data
                    break

            return packets

        except Exception as e:
            # This is normal when no data is available
            return None

    def decode_erlang_data(self, binary_data):
        """Decode Erlang binary term - simplified for development"""
        try:
            # For development, assume it's a string representation of Erlang terms
            text = binary_data.decode('utf-8', errors='ignore')
            if text.startswith('[') and (text.endswith(']') or text.endswith('].')):
                # Remove trailing period if present
                if text.endswith('].'):
                    text = text[:-1]
                # Use eval for development (not secure, but works for testing)
                return eval(text)
            return None
        except Exception as e:
            print(f"❌ Error decoding Erlang data: {e}")
            return None

    def handle_port_data(self, packets):
        """Handle received packets from Erlang ports"""
        for packet in packets:
            decoded_data = self.decode_erlang_data(packet)
            if decoded_data:
                if self.waiting_for_initial_map:
                    # First data should be from map_generator
                    print("🗺️ Received initial map from map_generator")
                    success = self.process_initial_map(decoded_data)
                    if success:
                        self.waiting_for_initial_map = False
                        self.map_initialized = True
                        print("✅ Initial map loaded! Now listening for cn_graphics_server updates...")
                else:
                    # Subsequent data from cn_graphics_server
                    print("🔄 Received update from cn_graphics_server")
                    self.process_map_update(decoded_data)

    def process_initial_map(self, map_data):
        """Process initial map from map_generator"""
        try:
            # Parse the map data into game state
            game_state = self.parse_complete_game_state(map_data)
            if game_state:
                self.current_game_state = game_state
                return True
            return False
        except Exception as e:
            print(f"❌ Error processing initial map: {e}")
            return False

    def process_map_update(self, update_data):
        """Process real-time update from cn_graphics_server"""
        try:
            # Store previous state for animation detection
            self.previous_game_state = self.current_game_state.copy() if self.current_game_state else None

            # Parse new state
            new_game_state = self.parse_complete_game_state(update_data)
            if new_game_state:
                self.current_game_state = new_game_state

                # Detect changes for animations
                if self.previous_game_state:
                    self.detect_complete_game_changes(self.previous_game_state, new_game_state)

                return True
            return False
        except Exception as e:
            print(f"❌ Error processing map update: {e}")
            return False

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

    def parse_complete_game_state(self, erlang_grid):
        """Parse complete game state including bombs, players, explosions"""
        game_state = {
            'tiles': [[0 for _ in range(MAP_SIZE)] for _ in range(MAP_SIZE)],
            'powerups': [['none' for _ in range(MAP_SIZE)] for _ in range(MAP_SIZE)],
            'bombs': [],  # List of active bombs with timers
            'players': [],  # Current player positions and status
            'explosions': [],  # Active explosion areas
            'game_info': {'time': 0, 'round': 1, 'status': 'playing'}
        }

        if not erlang_grid or not isinstance(erlang_grid, list):
            print("⚠️ Invalid grid data, using fallback")
            return self.get_fallback_game_state()

        for row_idx in range(len(erlang_grid)):
            if row_idx >= MAP_SIZE:
                break
            for col_idx in range(len(erlang_grid[row_idx])):
                if col_idx >= MAP_SIZE:
                    break

                cell = erlang_grid[row_idx][col_idx]

                # Handle both 4-tuple and 6-tuple formats
                if len(cell) >= 4:
                    tile_type, powerup_type, bomb_info, player_info = cell[:4]
                    explosion_info = cell[4] if len(cell) > 4 else 'none'
                    special_info = cell[5] if len(cell) > 5 else 'none'

                    # Transpose coordinates for display
                    x, y = col_idx, row_idx

                    # Parse tile
                    game_state['tiles'][x][y] = self.tile_mapping.get(tile_type, 0)
                    game_state['powerups'][x][y] = self.powerup_mapping.get(powerup_type, 'none')

                    # Parse bomb information
                    if bomb_info != 'none':
                        bomb_data = self.parse_bomb_info(bomb_info, x, y)
                        if bomb_data:
                            game_state['bombs'].append(bomb_data)

                    # Parse player information
                    if player_info != 'none':
                        player_data = self.parse_player_info(player_info, x, y)
                        if player_data:
                            game_state['players'].append(player_data)

                    # Parse explosion information
                    if explosion_info != 'none':
                        explosion_data = self.parse_explosion_info(explosion_info, x, y)
                        if explosion_data:
                            game_state['explosions'].append(explosion_data)

        print(
            f"✅ Game state loaded - Players: {len(game_state['players'])}, Bombs: {len(game_state['bombs'])}, Explosions: {len(game_state['explosions'])}")
        return game_state

    def parse_bomb_info(self, bomb_info, x, y):
        """Parse bomb information from Erlang data"""
        try:
            # Handle simple bomb atom like 'bomb' or complex tuple
            if isinstance(bomb_info, str) and 'bomb' in bomb_info.lower():
                return {
                    'x': x, 'y': y,
                    'type': 'normal_bomb',
                    'timer': 3,  # Default timer
                    'owner': 'unknown',
                    'power': 2,  # Default power
                    'animation_start': self.time
                }
            elif isinstance(bomb_info, tuple) and len(bomb_info) >= 2:
                # Complex bomb info: (bomb_type, timer, owner, power)
                bomb_type = bomb_info[0] if len(bomb_info) > 0 else 'normal_bomb'
                timer = int(bomb_info[1]) if len(bomb_info) > 1 and str(bomb_info[1]).isdigit() else 3
                owner = bomb_info[2] if len(bomb_info) > 2 else 'unknown'
                power = int(bomb_info[3]) if len(bomb_info) > 3 and str(bomb_info[3]).isdigit() else 2

                return {
                    'x': x, 'y': y,
                    'type': str(bomb_type),
                    'timer': timer,
                    'owner': str(owner),
                    'power': power,
                    'animation_start': self.time
                }
        except (ValueError, TypeError, IndexError):
            pass
        return None

    def parse_player_info(self, player_info, x, y):
        """Parse current player position and status"""
        try:
            # Handle simple player atom like 'player_1'
            if isinstance(player_info, str) and 'player_' in player_info:
                player_num = int(player_info.split('_')[1])
                return {
                    'player': player_num,
                    'x': x, 'y': y,
                    'health': 3,  # Default health
                    'status': 'alive',
                    'direction': 'north',
                    'last_update': self.time
                }
            elif isinstance(player_info, tuple) and len(player_info) >= 2:
                # Complex player info: (player_id, health, status, direction)
                player_id = player_info[0]
                health = int(player_info[1]) if len(player_info) > 1 and str(player_info[1]).isdigit() else 3
                status = player_info[2] if len(player_info) > 2 else 'alive'
                direction = player_info[3] if len(player_info) > 3 else 'north'

                if 'player_' in str(player_id):
                    player_num = int(str(player_id).split('_')[1])
                    return {
                        'player': player_num,
                        'x': x, 'y': y,
                        'health': health,
                        'status': str(status),
                        'direction': str(direction),
                        'last_update': self.time
                    }
        except (ValueError, TypeError, IndexError):
            pass
        return None

    def parse_explosion_info(self, explosion_info, x, y):
        """Parse explosion state information"""
        try:
            # Handle simple explosion atom
            if isinstance(explosion_info, str) and 'explosion' in explosion_info.lower():
                return {
                    'x': x, 'y': y,
                    'type': 'blast_center',
                    'intensity': 1.0,
                    'remaining_time': 0.5,
                    'start_time': self.time
                }
            elif isinstance(explosion_info, tuple) and len(explosion_info) >= 2:
                # Complex explosion info: (explosion_type, intensity, remaining_time)
                exp_type = explosion_info[0] if len(explosion_info) > 0 else 'blast_center'
                intensity = float(explosion_info[1]) if len(explosion_info) > 1 and str(explosion_info[1]).replace('.',
                                                                                                                   '').isdigit() else 1.0
                remaining = float(explosion_info[2]) if len(explosion_info) > 2 and str(explosion_info[2]).replace('.',
                                                                                                                   '').isdigit() else 0.5

                return {
                    'x': x, 'y': y,
                    'type': str(exp_type),
                    'intensity': intensity,
                    'remaining_time': remaining,
                    'start_time': self.time
                }
        except (ValueError, TypeError):
            pass
        return None

    def detect_complete_game_changes(self, old_state, new_state):
        """Comprehensive change detection for all game elements"""

        # Detect player movements with detailed tracking
        self.detect_detailed_player_changes(old_state.get('players', []),
                                            new_state.get('players', []))

        # Detect bomb lifecycle (placement, ticking, explosion)
        self.detect_bomb_lifecycle(old_state.get('bombs', []),
                                   new_state.get('bombs', []))

        # Detect explosion evolution
        self.detect_explosion_changes(old_state.get('explosions', []),
                                      new_state.get('explosions', []))

        # Detect tile changes (walls destroyed/created)
        self.detect_tile_changes(old_state['tiles'], new_state['tiles'])

        # Detect power-up changes
        self.detect_powerup_changes(old_state['powerups'], new_state['powerups'])

    def detect_detailed_player_changes(self, old_players, new_players):
        """Detect player movements, direction changes, status changes"""
        old_player_dict = {p['player']: p for p in old_players}
        new_player_dict = {p['player']: p for p in new_players}

        for player_id, new_player in new_player_dict.items():
            if player_id in old_player_dict:
                old_player = old_player_dict[player_id]

                # Position change - create walking animation
                if (old_player['x'], old_player['y']) != (new_player['x'], new_player['y']):
                    self.create_detailed_walking_animation(
                        player_id,
                        (old_player['x'], old_player['y']),
                        (new_player['x'], new_player['y']),
                        new_player.get('direction', 'north')
                    )
                    print(
                        f"🚶 Player {player_id} moved from ({old_player['x']}, {old_player['y']}) to ({new_player['x']}, {new_player['y']})")

                # Health change
                if old_player['health'] != new_player['health']:
                    if new_player['health'] < old_player['health']:
                        self.create_damage_effect(player_id, new_player['x'], new_player['y'])
                        print(f"💔 Player {player_id} took damage: {old_player['health']} -> {new_player['health']}")
                    else:
                        self.create_healing_effect(player_id, new_player['x'], new_player['y'])
                        print(f"💚 Player {player_id} healed: {old_player['health']} -> {new_player['health']}")

                # Status change (death, stun, etc.)
                if old_player['status'] != new_player['status']:
                    self.create_status_change_effect(player_id, new_player['x'], new_player['y'],
                                                     old_player['status'], new_player['status'])
                    print(f"⚡ Player {player_id} status change: {old_player['status']} -> {new_player['status']}")
            else:
                # New player appeared (respawn)
                print(f"✨ Player {player_id} spawned at ({new_player['x']}, {new_player['y']})")
                self.create_player_spawn_effect(player_id, new_player['x'], new_player['y'])

    def detect_bomb_lifecycle(self, old_bombs, new_bombs):
        """Detect bomb placement, countdown, and detonation"""
        old_bomb_positions = {(b['x'], b['y']): b for b in old_bombs}
        new_bomb_positions = {(b['x'], b['y']): b for b in new_bombs}

        # New bombs placed
        for pos, bomb in new_bomb_positions.items():
            if pos not in old_bomb_positions:
                self.create_bomb_placement_animation(bomb['x'], bomb['y'], bomb)
                print(f"💣 Bomb placed at {pos} by {bomb['owner']}")

        # Bombs that exploded (disappeared)
        for pos, bomb in old_bomb_positions.items():
            if pos not in new_bomb_positions:
                self.create_bomb_explosion_sequence(bomb['x'], bomb['y'], bomb)
                print(f"💥 Bomb exploded at {pos}")

        # Bomb timer updates
        for pos in old_bomb_positions:
            if pos in new_bomb_positions:
                old_timer = old_bomb_positions[pos]['timer']
                new_timer = new_bomb_positions[pos]['timer']
                if old_timer != new_timer:
                    self.update_bomb_countdown_animation(pos[0], pos[1], new_timer)

    def detect_explosion_changes(self, old_explosions, new_explosions):
        """Detect explosion evolution"""
        old_explosion_positions = {(e['x'], e['y']): e for e in old_explosions}
        new_explosion_positions = {(e['x'], e['y']): e for e in new_explosions}

        # New explosions
        for pos, explosion in new_explosion_positions.items():
            if pos not in old_explosion_positions:
                self.create_live_explosion_effect(explosion['x'], explosion['y'], explosion)
                print(f"🔥 Explosion started at {pos}")

    def detect_tile_changes(self, old_tiles, new_tiles):
        """Detect tile changes (walls destroyed/created)"""
        for x in range(MAP_SIZE):
            for y in range(MAP_SIZE):
                old_tile = old_tiles[x][y]
                new_tile = new_tiles[x][y]

                if old_tile != new_tile:
                    self.create_tile_change_effect(x, y, old_tile, new_tile)
                    if old_tile == 1 and new_tile == 0:  # Breakable destroyed
                        print(f"🧱 Breakable wall destroyed at ({x}, {y})")

    def detect_powerup_changes(self, old_powerups, new_powerups):
        """Detect power-up pickup and spawn"""
        for x in range(MAP_SIZE):
            for y in range(MAP_SIZE):
                old_powerup = old_powerups[x][y]
                new_powerup = new_powerups[x][y]

                # Power-up was picked up
                if old_powerup != 'none' and new_powerup == 'none':
                    self.create_powerup_pickup_animation(x, y, old_powerup)
                    print(f"✨ Power-up '{old_powerup}' picked up at ({x}, {y})")

                # New power-up appeared
                elif old_powerup == 'none' and new_powerup != 'none':
                    self.create_powerup_spawn_animation(x, y, new_powerup)
                    print(f"🎁 Power-up '{new_powerup}' spawned at ({x}, {y})")

    # Animation Creation Methods
    def create_detailed_walking_animation(self, player_id, old_pos, new_pos, direction):
        """Enhanced walking animation with direction and footsteps"""
        self.player_animations[player_id] = {
            'type': 'walking',
            'start_pos': old_pos,
            'end_pos': new_pos,
            'direction': direction,
            'start_time': self.time,
            'duration': 0.3,  # Smooth movement
            'footstep_sound': True,
            'active': True
        }

        # Add dust cloud effect at start position
        self.game_effects.append({
            'type': 'dust_cloud',
            'x': old_pos[0], 'y': old_pos[1],
            'start_time': self.time,
            'duration': 0.3,
            'active': True
        })

    def create_bomb_placement_animation(self, x, y, bomb_data):
        """Animation for bomb being placed"""
        self.bomb_animations[(x, y)] = {
            'type': 'placement',
            'timer': bomb_data['timer'],
            'max_timer': bomb_data['timer'],
            'owner': bomb_data['owner'],
            'power': bomb_data['power'],
            'start_time': self.time,
            'pulse_phase': 0,
            'active': True
        }

        # Screen shake for bomb placement
        self.camera_shake = 0.1

    def create_bomb_explosion_sequence(self, x, y, bomb_data):
        """Complete explosion animation sequence"""
        # Central explosion
        self.explosion_animations.append({
            'type': 'bomb_center',
            'x': x, 'y': y,
            'power': bomb_data['power'],
            'start_time': self.time,
            'duration': 1.5,
            'active': True
        })

        # Explosion rays in 4 directions
        for direction in ['north', 'south', 'east', 'west']:
            for distance in range(1, bomb_data['power'] + 1):
                ray_x, ray_y = self.calculate_explosion_ray_position(x, y, direction, distance)
                if 0 <= ray_x < MAP_SIZE and 0 <= ray_y < MAP_SIZE:
                    self.explosion_animations.append({
                        'type': 'explosion_ray',
                        'x': ray_x, 'y': ray_y,
                        'direction': direction,
                        'distance': distance,
                        'start_time': self.time + distance * 0.05,  # Staggered timing
                        'duration': 1.0,
                        'active': True
                    })

        # Major camera shake
        self.camera_shake = 0.8

    def calculate_explosion_ray_position(self, x, y, direction, distance):
        """Calculate position of explosion ray"""
        directions = {
            'north': (0, -1), 'south': (0, 1),
            'east': (1, 0), 'west': (-1, 0)
        }
        dx, dy = directions[direction]
        return x + dx * distance, y + dy * distance

    def update_bomb_countdown_animation(self, x, y, new_timer):
        """Update bomb countdown animation"""
        if (x, y) in self.bomb_animations:
            self.bomb_animations[(x, y)]['timer'] = new_timer
            # Increase pulse speed as timer decreases
            self.bomb_animations[(x, y)]['pulse_speed'] = 4.0 - new_timer

    def create_live_explosion_effect(self, x, y, explosion_data):
        """Create live explosion effect from game state"""
        self.explosion_animations.append({
            'type': explosion_data['type'],
            'x': x, 'y': y,
            'intensity': explosion_data['intensity'],
            'start_time': self.time,
            'duration': explosion_data['remaining_time'],
            'active': True
        })

    def create_damage_effect(self, player_id, x, y):
        """Effect when player takes damage"""
        self.game_effects.append({
            'type': 'damage',
            'player_id': player_id,
            'x': x, 'y': y,
            'start_time': self.time,
            'duration': 0.8,
            'flash_color': (255, 0, 0),
            'active': True
        })

    def create_healing_effect(self, player_id, x, y):
        """Effect when player heals"""
        self.game_effects.append({
            'type': 'healing',
            'player_id': player_id,
            'x': x, 'y': y,
            'start_time': self.time,
            'duration': 1.0,
            'active': True
        })

    def create_status_change_effect(self, player_id, x, y, old_status, new_status):
        """Effect for player status changes"""
        if new_status == 'dead':
            self.game_effects.append({
                'type': 'player_death',
                'player_id': player_id,
                'x': x, 'y': y,
                'start_time': self.time,
                'duration': 2.0,
                'active': True
            })
        elif new_status == 'stunned':
            self.status_effects[player_id] = {
                'type': 'stun',
                'start_time': self.time,
                'duration': 3.0  # Assume 3 second stun
            }

    def create_player_spawn_effect(self, player_id, x, y):
        """Effect when player spawns/respawns"""
        self.game_effects.append({
            'type': 'player_spawn',
            'player_id': player_id,
            'x': x, 'y': y,
            'start_time': self.time,
            'duration': 1.5,
            'active': True
        })

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

    def update_all_animations(self):
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

        # Update bomb animations
        for pos in list(self.bomb_animations.keys()):
            anim = self.bomb_animations[pos]
            elapsed = current_time - anim['start_time']
            # Keep bomb animations active until explicitly removed
            if elapsed > 10:  # Safety timeout
                del self.bomb_animations[pos]

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

        # Update status effects
        for player_id in list(self.status_effects.keys()):
            effect = self.status_effects[player_id]
            if current_time - effect['start_time'] > effect['duration']:
                del self.status_effects[player_id]

        # Update camera shake
        if self.camera_shake > 0:
            self.camera_shake -= 1 / FPS

    def load_player_stats(self):
        """Load player statistics - will be enhanced to read from game state"""
        return {
            1: {
                'life': 3, 'speed': 1, 'bombs': 3, 'explosion_radius': 2,
                'special_abilities': [], 'color': COLORS['PLAYER_1']
            },
            2: {
                'life': 2, 'speed': 2, 'bombs': 4, 'explosion_radius': 1,
                'special_abilities': ['kick_bomb'], 'color': COLORS['PLAYER_2']
            },
            3: {
                'life': 4, 'speed': 1, 'bombs': 2, 'explosion_radius': 3,
                'special_abilities': [], 'color': COLORS['PLAYER_3']
            },
            4: {
                'life': 1, 'speed': 3, 'bombs': 5, 'explosion_radius': 1,
                'special_abilities': ['plus_bombs', 'phased', 'freeze_bomb'], 'color': COLORS['PLAYER_4']
            }
        }

    def get_fallback_game_state(self):
        """Fallback game state if no data received"""
        return {
            'tiles': [[0 for _ in range(MAP_SIZE)] for _ in range(MAP_SIZE)],
            'powerups': [['none' for _ in range(MAP_SIZE)] for _ in range(MAP_SIZE)],
            'bombs': [],
            'players': [],
            'explosions': [],
            'game_info': {'time': 0, 'round': 1, 'status': 'waiting'}
        }

    # Drawing utility methods
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
        self.draw_enhanced_shadow(surface, x, y, TILE_SIZE, TILE_SIZE, 80)

        rect = pygame.Rect(x, y, TILE_SIZE, TILE_SIZE)
        self.draw_gradient_rect(surface, COLORS['BRICK_TOP'], COLORS['BRICK_DARK'], rect)

        # Brick pattern
        brick_height = TILE_SIZE // 4
        for row in range(4):
            brick_y = y + row * brick_height
            pygame.draw.line(surface, COLORS['MORTAR'],
                             (x, brick_y), (x + TILE_SIZE, brick_y), 2)

            offset = (TILE_SIZE // 3) if row % 2 == 0 else 0
            for i in range(3):
                brick_x = x + offset + i * (TILE_SIZE // 3)
                if x <= brick_x < x + TILE_SIZE:
                    pygame.draw.line(surface, COLORS['MORTAR'],
                                     (brick_x, brick_y), (brick_x, brick_y + brick_height), 2)

        pygame.draw.line(surface, COLORS['BRICK_TOP'], (x, y), (x + TILE_SIZE, y), 2)
        pygame.draw.line(surface, COLORS['BRICK_TOP'], (x, y), (x, y + TILE_SIZE), 2)

    def draw_enhanced_wooden_barrel(self, surface, x, y, has_powerup=False):
        """Wooden barrel with realistic shading"""
        self.draw_enhanced_shadow(surface, x, y, TILE_SIZE, TILE_SIZE, 70)

        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2

        # Barrel body with curve
        for i in range(TILE_SIZE):
            y_pos = y + i
            curve_factor = 1.0 + 0.2 * math.sin((i / TILE_SIZE) * math.pi)
            width = int((TILE_SIZE - 12) * curve_factor)

            ratio = i / TILE_SIZE
            r = int(COLORS['WOOD_LIGHT'][0] * (1 - ratio) + COLORS['WOOD_DARK'][0] * ratio)
            g = int(COLORS['WOOD_LIGHT'][1] * (1 - ratio) + COLORS['WOOD_DARK'][1] * ratio)
            b = int(COLORS['WOOD_LIGHT'][2] * (1 - ratio) + COLORS['WOOD_DARK'][2] * ratio)

            pygame.draw.line(surface, (r, g, b),
                             (center_x - width // 2, y_pos), (center_x + width // 2, y_pos), 1)

        # Metal bands
        band_positions = [0.2, 0.5, 0.8]
        for band_ratio in band_positions:
            band_y = int(y + TILE_SIZE * band_ratio)
            band_width = int((TILE_SIZE - 8) * (1.0 + 0.2 * math.sin(band_ratio * math.pi)))

            pygame.draw.rect(surface, COLORS['WOOD_SHADOW'],
                             (center_x - band_width // 2, band_y - 2, band_width, 5))
            pygame.draw.rect(surface, COLORS['WOOD_BAND'],
                             (center_x - band_width // 2, band_y - 3, band_width, 4))
            pygame.draw.rect(surface, COLORS['WOOD_HIGHLIGHT'],
                             (center_x - band_width // 2, band_y - 3, band_width, 1))

        # Wood grain
        for i in range(6):
            grain_x = x + 8 + i * 6
            if grain_x < x + TILE_SIZE - 8:
                pygame.draw.line(surface, COLORS['WOOD_SHADOW'],
                                 (grain_x, y + 6), (grain_x, y + TILE_SIZE - 6), 1)

        pygame.draw.line(surface, COLORS['WOOD_HIGHLIGHT'],
                         (x + 6, y + 4), (x + 6, y + TILE_SIZE - 4), 2)

        if has_powerup:
            self.draw_powerup_glow(surface, center_x, center_y)

    def draw_enhanced_metal_barrel(self, surface, x, y, has_powerup=False):
        """Shiny metal barrel with reflections"""
        self.draw_enhanced_shadow(surface, x, y, TILE_SIZE, TILE_SIZE, 70)

        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2

        # Barrel body with metallic gradient
        for i in range(TILE_SIZE):
            y_pos = y + i
            curve_factor = 1.0 + 0.2 * math.sin((i / TILE_SIZE) * math.pi)
            width = int((TILE_SIZE - 12) * curve_factor)

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

        # Metallic shine
        shine_width = 4
        pygame.draw.rect(surface, COLORS['METAL_SHINE'],
                         (center_x - 8, y + 6, shine_width, TILE_SIZE - 12))
        pygame.draw.rect(surface, COLORS['METAL_LIGHT'],
                         (center_x + 4, y + 8, 2, TILE_SIZE - 16))

        if has_powerup:
            self.draw_powerup_glow(surface, center_x, center_y)

    def draw_powerup_glow(self, surface, center_x, center_y):
        """Power-up glow effect"""
        glow_intensity = 0.7 + 0.3 * math.sin(self.powerup_pulse * 4)
        glow_size = int(20 + 8 * math.sin(self.powerup_pulse * 3))

        for radius in range(glow_size, 0, -3):
            alpha = int(30 * glow_intensity * (radius / glow_size))
            if alpha > 0:
                glow_surf = pygame.Surface((radius * 2, radius * 2), pygame.SRCALPHA)
                pygame.draw.circle(glow_surf, (*COLORS['POWERUP_GLOW'], alpha),
                                   (radius, radius), radius)
                surface.blit(glow_surf, (center_x - radius, center_y - radius))

    def draw_animated_bomb(self, surface, x, y, bomb_data):
        """Draw animated bomb with countdown"""
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2

        # Get bomb animation data
        bomb_anim = self.bomb_animations.get((bomb_data['x'], bomb_data['y']))

        # Pulsing effect based on timer
        pulse_speed = 2.0 + (bomb_data['timer'] * 2)  # Faster pulse as timer decreases
        pulse = 0.8 + 0.2 * math.sin(self.time * pulse_speed)
        bomb_size = int(16 * pulse)

        # Danger glow (increases as timer decreases)
        if bomb_data['timer'] <= 1:
            danger_intensity = 1.0
        elif bomb_data['timer'] <= 2:
            danger_intensity = 0.7
        else:
            danger_intensity = 0.3

        # Draw danger glow
        glow_size = int(bomb_size * 2 * danger_intensity)
        glow_surf = pygame.Surface((glow_size * 2, glow_size * 2), pygame.SRCALPHA)
        pygame.draw.circle(glow_surf, (*COLORS['BOMB_FUSE'], int(80 * danger_intensity)),
                           (glow_size, glow_size), glow_size)
        surface.blit(glow_surf, (center_x - glow_size, center_y - glow_size))

        # Drop shadow
        shadow_surf = pygame.Surface((bomb_size * 2, bomb_size * 2), pygame.SRCALPHA)
        pygame.draw.circle(shadow_surf, (0, 0, 0, 100), (bomb_size, bomb_size), bomb_size)
        surface.blit(shadow_surf, (center_x - bomb_size + 2, center_y - bomb_size + 2))

        # Main bomb body
        pygame.draw.circle(surface, COLORS['BOMB_BLACK'], (center_x, center_y), bomb_size)
        pygame.draw.circle(surface, (60, 60, 60), (center_x, center_y), bomb_size, 2)

        # Highlight
        pygame.draw.circle(surface, (100, 100, 100), (center_x - bomb_size // 3, center_y - bomb_size // 3),
                           bomb_size // 4)

        # Fuse
        fuse_length = bomb_size // 2
        fuse_end_x = center_x - fuse_length
        fuse_end_y = center_y - bomb_size
        pygame.draw.line(surface, COLORS['BOMB_FUSE'], (center_x, center_y - bomb_size),
                         (fuse_end_x, fuse_end_y), 3)

        # Sparking fuse tip
        spark_intensity = 0.5 + 0.5 * math.sin(self.time * 10)
        spark_size = int(4 * spark_intensity)
        if spark_size > 0:
            pygame.draw.circle(surface, COLORS['BOMB_FUSE'], (fuse_end_x, fuse_end_y), spark_size)
            pygame.draw.circle(surface, (255, 255, 0), (fuse_end_x, fuse_end_y), spark_size // 2)

        # Timer display
        timer_text = str(bomb_data['timer'])
        timer_surface = self.font.render(timer_text, True, (255, 255, 255))
        timer_rect = timer_surface.get_rect(center=(center_x, center_y + bomb_size + 15))

        # Timer background
        bg_rect = timer_rect.inflate(8, 4)
        bg_surf = pygame.Surface((bg_rect.width, bg_rect.height), pygame.SRCALPHA)
        pygame.draw.rect(bg_surf, (0, 0, 0, 150), (0, 0, bg_rect.width, bg_rect.height))
        surface.blit(bg_surf, bg_rect.topleft)
        surface.blit(timer_surface, timer_rect)

    def draw_enhanced_player_with_effects(self, surface, x, y, player_data):
        """Draw player with all status effects and animations"""
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2

        player_num = player_data['player']

        # Get player color
        player_colors = {
            1: COLORS['PLAYER_1'], 2: COLORS['PLAYER_2'],
            3: COLORS['PLAYER_3'], 4: COLORS['PLAYER_4']
        }
        outfit_color = player_colors.get(player_num, COLORS['PLAYER_1'])

        # Check for walking animation
        char_x, char_y = x, y
        if player_num in self.player_animations:
            anim = self.player_animations[player_num]
            elapsed = self.time - anim['start_time']
            progress = min(elapsed / anim['duration'], 1.0)

            # Interpolate position
            start_x, start_y = anim['start_pos']
            end_x, end_y = anim['end_pos']

            current_x = start_x + (end_x - start_x) * progress
            current_y = start_y + (end_y - start_y) * progress

            # Convert to screen coordinates
            char_x = current_y * TILE_SIZE
            char_y = current_x * TILE_SIZE
            center_x = char_x + TILE_SIZE // 2
            center_y = char_y + TILE_SIZE // 2

            # Walking bounce
            walk_bounce = math.sin(progress * math.pi * 6) * 3
            center_y -= walk_bounce

        # Status effects
        status_effect = self.status_effects.get(player_num)

        # Damage flash effect
        flash_alpha = 0
        for effect in self.game_effects:
            if (effect.get('type') == 'damage' and
                    effect.get('player_id') == player_num):
                elapsed = self.time - effect['start_time']
                if elapsed < effect['duration']:
                    flash_alpha = int(150 * (1 - elapsed / effect['duration']) * math.sin(elapsed * 20))

        # Death effect
        death_fade = 1.0
        for effect in self.game_effects:
            if (effect.get('type') == 'player_death' and
                    effect.get('player_id') == player_num):
                elapsed = self.time - effect['start_time']
                if elapsed < effect['duration']:
                    death_fade = max(0.0, 1.0 - elapsed / effect['duration'])

        # Spawn effect
        spawn_glow = 0
        for effect in self.game_effects:
            if (effect.get('type') == 'player_spawn' and
                    effect.get('player_id') == player_num):
                elapsed = self.time - effect['start_time']
                if elapsed < effect['duration']:
                    spawn_glow = int(100 * (1 - elapsed / effect['duration']))

        # Apply effects to colors
        if death_fade < 1.0:
            outfit_color = tuple(int(c * death_fade) for c in outfit_color)

        # Draw player
        self.draw_enhanced_player_character(surface, char_x, char_y, player_num, outfit_color)

        # Draw status effect overlays
        if status_effect and status_effect['type'] == 'stun':
            # Draw stun stars around player
            for i in range(3):
                angle = (self.time * 4 + i * 2.1) % (2 * math.pi)
                star_x = center_x + int(25 * math.cos(angle))
                star_y = center_y + int(25 * math.sin(angle)) - 20
                self.draw_stun_star(surface, star_x, star_y)

        # Flash overlay for damage
        if flash_alpha > 0:
            flash_surf = pygame.Surface((TILE_SIZE, TILE_SIZE), pygame.SRCALPHA)
            pygame.draw.rect(flash_surf, (255, 0, 0, flash_alpha), (0, 0, TILE_SIZE, TILE_SIZE))
            surface.blit(flash_surf, (char_x, char_y))

        # Spawn glow
        if spawn_glow > 0:
            glow_surf = pygame.Surface((TILE_SIZE * 2, TILE_SIZE * 2), pygame.SRCALPHA)
            pygame.draw.circle(glow_surf, (255, 255, 255, spawn_glow),
                               (TILE_SIZE, TILE_SIZE), TILE_SIZE)
            surface.blit(glow_surf, (char_x - TILE_SIZE // 2, char_y - TILE_SIZE // 2))

    def draw_enhanced_player_character(self, surface, x, y, player_num, outfit_color):
        """Draw the actual player character (enhanced version)"""
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2

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

        # Eyes
        pygame.draw.ellipse(surface, (255, 255, 255), (center_x - 6, head_y - 4, 6, 4))
        pygame.draw.circle(surface, (0, 0, 0), (center_x - 3, head_y - 2), 2)
        pygame.draw.circle(surface, (255, 255, 255), (center_x - 2, head_y - 3), 1)

        pygame.draw.ellipse(surface, (255, 255, 255), (center_x + 1, head_y - 4, 6, 4))
        pygame.draw.circle(surface, (0, 0, 0), (center_x + 4, head_y - 2), 2)
        pygame.draw.circle(surface, (255, 255, 255), (center_x + 5, head_y - 3), 1)

        # Eyebrows
        pygame.draw.arc(surface, (101, 67, 33), (center_x - 6, head_y - 7, 5, 4), 0, math.pi, 2)
        pygame.draw.arc(surface, (101, 67, 33), (center_x + 2, head_y - 7, 5, 4), 0, math.pi, 2)

        # Nose and mouth
        pygame.draw.circle(surface, (200, 150, 120), (center_x, head_y), 1)
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

        # Player number badge
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

    def draw_stun_star(self, surface, x, y):
        """Draw a stun star effect"""
        star_size = 6
        star_color = (255, 255, 100)

        # Draw star shape
        points = []
        for i in range(10):  # 5-pointed star = 10 points
            angle = i * math.pi / 5
            if i % 2 == 0:
                # Outer points
                px = x + star_size * math.cos(angle)
                py = y + star_size * math.sin(angle)
            else:
                # Inner points
                px = x + (star_size // 2) * math.cos(angle)
                py = y + (star_size // 2) * math.sin(angle)
            points.append((px, py))

        pygame.draw.polygon(surface, star_color, points)
        pygame.draw.polygon(surface, (255, 255, 255), points, 1)

    # Explosion drawing methods
    def draw_explosion_effect(self, surface, explosion):
        """Draw various types of explosion effects"""
        elapsed = self.time - explosion['start_time']
        progress = elapsed / explosion['duration']

        if explosion['type'] == 'bomb_center':
            self.draw_bomb_center_explosion(surface, explosion, progress)
        elif explosion['type'] == 'explosion_ray':
            self.draw_explosion_ray(surface, explosion, progress)
        elif explosion['type'] in ['blast_center', 'blast_horizontal', 'blast_vertical']:
            self.draw_live_explosion(surface, explosion, progress)

    def draw_bomb_center_explosion(self, surface, explosion, progress):
        """Draw central bomb explosion"""
        center_x = explosion['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = explosion['x'] * TILE_SIZE + TILE_SIZE // 2

        if progress < 0.3:  # Expanding blast
            radius = int(progress * 80)
            colors = [COLORS['EXPLOSION_CORE'], COLORS['EXPLOSION_MIDDLE'], COLORS['EXPLOSION_OUTER']]

            for i, color in enumerate(colors):
                explosion_radius = max(1, radius - i * 8)
                if explosion_radius > 0:
                    explosion_surf = pygame.Surface((explosion_radius * 2, explosion_radius * 2), pygame.SRCALPHA)
                    alpha = int(255 * (1 - progress * 2))
                    pygame.draw.circle(explosion_surf, (*color, alpha),
                                       (explosion_radius, explosion_radius), explosion_radius)
                    surface.blit(explosion_surf, (center_x - explosion_radius, center_y - explosion_radius))

        elif progress < 0.8:  # Fire and smoke
            # Fire particles
            for _ in range(12):
                angle = random.random() * 2 * math.pi
                distance = random.random() * 50
                particle_x = center_x + int(math.cos(angle) * distance)
                particle_y = center_y + int(math.sin(angle) * distance)
                particle_size = random.randint(3, 8)

                fire_intensity = 1 - (progress - 0.3) / 0.5
                color = (255, int(random.randint(100, 255) * fire_intensity),
                         int(random.randint(0, 100) * fire_intensity))
                pygame.draw.circle(surface, color, (particle_x, particle_y), particle_size)

    def draw_explosion_ray(self, surface, explosion, progress):
        """Draw explosion ray extending from bomb"""
        if progress > 1.0:
            return

        center_x = explosion['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = explosion['x'] * TILE_SIZE + TILE_SIZE // 2

        # Explosion ray effect
        intensity = 1.0 - progress
        size = int(TILE_SIZE * 0.8 * intensity)

        if size > 0:
            # Main explosion circle
            explosion_surf = pygame.Surface((size * 2, size * 2), pygame.SRCALPHA)
            alpha = int(200 * intensity)

            # Gradient explosion
            for radius in range(size, 0, -2):
                ratio = radius / size
                if ratio > 0.7:
                    color = COLORS['EXPLOSION_CORE']
                elif ratio > 0.4:
                    color = COLORS['EXPLOSION_MIDDLE']
                else:
                    color = COLORS['EXPLOSION_OUTER']

                current_alpha = int(alpha * ratio)
                pygame.draw.circle(explosion_surf, (*color, current_alpha), (size, size), radius)

            surface.blit(explosion_surf, (center_x - size, center_y - size))

    def draw_live_explosion(self, surface, explosion, progress):
        """Draw live explosion from game state"""
        center_x = explosion['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = explosion['x'] * TILE_SIZE + TILE_SIZE // 2

        intensity = explosion['intensity'] * (1.0 - progress)
        size = int(TILE_SIZE * intensity)

        if size > 0:
            # Pulsing explosion effect
            pulse = 1.0 + 0.3 * math.sin(self.time * 15)
            actual_size = int(size * pulse)

            explosion_surf = pygame.Surface((actual_size * 2, actual_size * 2), pygame.SRCALPHA)
            alpha = int(150 * intensity)

            pygame.draw.circle(explosion_surf, (*COLORS['EXPLOSION_MIDDLE'], alpha),
                               (actual_size, actual_size), actual_size)
            pygame.draw.circle(explosion_surf, (*COLORS['EXPLOSION_CORE'], alpha // 2),
                               (actual_size, actual_size), actual_size // 2)

            surface.blit(explosion_surf, (center_x - actual_size, center_y - actual_size))

    # Effect drawing methods
    def draw_powerup_pickup_effect(self, surface, animation):
        """Draw power-up pickup animation"""
        elapsed = self.time - animation['start_time']
        progress = elapsed / animation['duration']

        center_x = animation['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = animation['x'] * TILE_SIZE + TILE_SIZE // 2

        # Rising sparkles
        for i in range(8):
            sparkle_y = center_y - int(progress * 80) - i * 10
            sparkle_x = center_x + int(math.sin(self.time * 5 + i) * 20)

            if sparkle_y > center_y - 100:
                alpha = int(255 * (1 - progress))
                sparkle_size = max(1, int(4 * (1 - progress)))
                color = (*COLORS['POWERUP_CORE'][:3], alpha)
                sparkle_surf = pygame.Surface((sparkle_size * 2, sparkle_size * 2), pygame.SRCALPHA)
                pygame.draw.circle(sparkle_surf, color, (sparkle_size, sparkle_size), sparkle_size)
                surface.blit(sparkle_surf, (sparkle_x - sparkle_size, sparkle_y - sparkle_size))

    def draw_dust_cloud_effect(self, surface, effect):
        """Draw dust cloud from player movement"""
        elapsed = self.time - effect['start_time']
        progress = elapsed / effect['duration']

        center_x = effect['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = effect['x'] * TILE_SIZE + TILE_SIZE // 2

        # Expanding dust cloud
        cloud_size = int(progress * 30)
        alpha = int(60 * (1 - progress))

        if cloud_size > 0 and alpha > 0:
            dust_surf = pygame.Surface((cloud_size * 2, cloud_size * 2), pygame.SRCALPHA)
            pygame.draw.circle(dust_surf, (200, 180, 150, alpha),
                               (cloud_size, cloud_size), cloud_size)
            surface.blit(dust_surf, (center_x - cloud_size, center_y - cloud_size))

    def draw_healing_effect(self, surface, effect):
        """Draw healing effect"""
        elapsed = self.time - effect['start_time']
        progress = elapsed / effect['duration']

        center_x = effect['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = effect['x'] * TILE_SIZE + TILE_SIZE // 2

        # Rising green sparkles
        for i in range(5):
            sparkle_y = center_y - int(progress * 60) - i * 12
            sparkle_x = center_x + int(math.sin(self.time * 4 + i) * 15)

            if sparkle_y > center_y - 80:
                alpha = int(200 * (1 - progress))
                sparkle_size = 3
                pygame.draw.circle(surface, (100, 255, 100, alpha),
                                   (sparkle_x, sparkle_y), sparkle_size)

    # Custom icon drawing
    def draw_custom_icon(self, surface, icon_type, center_x, center_y, size, color):
        """Icons for power-ups"""
        if icon_type == "lightning":
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
            pygame.draw.rect(surface, color, (center_x - size // 3, center_y - size // 2, size // 1.5, size))
            pygame.draw.circle(surface, (255, 100, 100), (center_x - size // 6, center_y - size // 4), 3)
            pygame.draw.circle(surface, (100, 255, 100), (center_x + size // 6, center_y - size // 4), 3)
            pygame.draw.line(surface, color, (center_x, center_y - size // 2), (center_x, center_y - size), 2)

        elif icon_type == "factory":
            pygame.draw.circle(surface, color, (center_x - size // 6, center_y), size // 2)
            pygame.draw.line(surface, color, (center_x - size // 6, center_y - size // 2),
                             (center_x - size // 3, center_y - size), 2)
            pygame.draw.circle(surface, (255, 200, 0), (center_x - size // 3, center_y - size), 2)
            pygame.draw.circle(surface, color, (center_x + size // 4, center_y + size // 6), size // 4)
            pygame.draw.line(surface, color, (center_x + size // 4, center_y + size // 6 - size // 4),
                             (center_x + size // 6, center_y - size // 3), 1)
            pygame.draw.circle(surface, (255, 200, 0), (center_x + size // 6, center_y - size // 3), 1)

        elif icon_type == "boot":
            pygame.draw.line(surface, color, (center_x - size // 3, center_y - size // 4),
                             (center_x, center_y + size // 4), 8)
            pygame.draw.line(surface, color, (center_x, center_y + size // 4),
                             (center_x + size // 2, center_y + size // 6), 6)
            pygame.draw.ellipse(surface, color, (center_x + size // 3, center_y, size // 1.5, size // 3))
            for i in range(3):
                line_x = center_x + size // 2 + i * 6
                pygame.draw.line(surface, (*color[:3], 150), (line_x, center_y + size // 8),
                                 (line_x + 8, center_y + size // 8), 2)

        elif icon_type == "ghost":
            pygame.draw.circle(surface, color, (center_x, center_y - size // 4), size // 2)
            pygame.draw.rect(surface, color, (center_x - size // 2, center_y - size // 4, size, size // 2))
            wave_points = []
            for i in range(5):
                x = center_x - size // 2 + i * (size // 4)
                y = center_y + size // 4 + (5 if i % 2 == 0 else -5)
                wave_points.append((x, y))
            if len(wave_points) > 2:
                pygame.draw.polygon(surface, color, [(center_x - size // 2, center_y + size // 4)] + wave_points + [
                    (center_x + size // 2, center_y + size // 4)])

        elif icon_type == "bomb":
            pygame.draw.circle(surface, color, (center_x, center_y), size // 2)
            pygame.draw.line(surface, color, (center_x, center_y - size // 2), (center_x - size // 4, center_y - size),
                             3)
            pygame.draw.circle(surface, (255, 200, 0), (center_x - size // 4, center_y - size), 3)

        elif icon_type == "explosion":
            for i in range(8):
                angle = i * math.pi / 4
                x = center_x + int(size // 2 * math.cos(angle))
                y = center_y + int(size // 2 * math.sin(angle))
                pygame.draw.line(surface, color, (center_x, center_y), (x, y), 3)
                pygame.draw.circle(surface, color, (x, y), 3)
            pygame.draw.circle(surface, color, (center_x, center_y), size // 4)

        elif icon_type == "heart":
            pygame.draw.circle(surface, color, (center_x - size // 4, center_y - size // 6), size // 3)
            pygame.draw.circle(surface, color, (center_x + size // 4, center_y - size // 6), size // 3)
            points = [
                (center_x - size // 2, center_y),
                (center_x + size // 2, center_y),
                (center_x, center_y + size // 2)
            ]
            pygame.draw.polygon(surface, color, points)

        elif icon_type == "freeze":
            pygame.draw.line(surface, color, (center_x, center_y - size // 2), (center_x, center_y + size // 2), 3)
            pygame.draw.line(surface, color, (center_x - size // 2, center_y), (center_x + size // 2, center_y), 3)
            pygame.draw.line(surface, color, (center_x - size // 3, center_y - size // 3),
                             (center_x + size // 3, center_y + size // 3), 3)
            pygame.draw.line(surface, color, (center_x - size // 3, center_y + size // 3),
                             (center_x + size // 3, center_y - size // 3), 3)

    # Main drawing methods
    def draw_enhanced_map(self):
        """Draw the complete enhanced map with all animations"""
        # Apply camera shake
        shake_x = int(random.random() * self.camera_shake * 10) if self.camera_shake > 0 else 0
        shake_y = int(random.random() * self.camera_shake * 10) if self.camera_shake > 0 else 0

        self.map_surface.fill(COLORS['BACKGROUND'])

        # Update animations
        self.time += 1 / FPS
        self.powerup_pulse += 1 / FPS
        self.update_all_animations()

        # Draw tiles with shake offset
        for x in range(MAP_SIZE):
            for y in range(MAP_SIZE):
                pixel_x = y * TILE_SIZE + shake_x
                pixel_y = x * TILE_SIZE + shake_y

                tile_type = self.current_game_state['tiles'][x][y]
                powerup = self.current_game_state['powerups'][x][y]
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

                # Selection highlight
                if self.selected_tile == (x, y):
                    highlight_surf = pygame.Surface((TILE_SIZE, TILE_SIZE), pygame.SRCALPHA)
                    pygame.draw.rect(highlight_surf, COLORS['SELECTION'], (0, 0, TILE_SIZE, TILE_SIZE))
                    self.map_surface.blit(highlight_surf, (pixel_x, pixel_y))

        # Draw bombs
        for bomb in self.current_game_state['bombs']:
            pixel_x = bomb['y'] * TILE_SIZE + shake_x
            pixel_y = bomb['x'] * TILE_SIZE + shake_y
            self.draw_animated_bomb(self.map_surface, pixel_x, pixel_y, bomb)

        # Draw players
        for player in self.current_game_state['players']:
            pixel_x = player['y'] * TILE_SIZE + shake_x
            pixel_y = player['x'] * TILE_SIZE + shake_y
            self.draw_enhanced_player_with_effects(self.map_surface, pixel_x, pixel_y, player)

        # Draw all active explosions
        for explosion in self.explosion_animations:
            self.draw_explosion_effect(self.map_surface, explosion)

        # Draw all active game effects
        for effect in self.game_effects:
            if effect.get('type') == 'dust_cloud':
                self.draw_dust_cloud_effect(self.map_surface, effect)
            elif effect.get('type') == 'healing':
                self.draw_healing_effect(self.map_surface, effect)

        # Draw power-up animations
        for powerup_anim in self.powerup_animations:
            if powerup_anim['type'] == 'pickup':
                self.draw_powerup_pickup_effect(self.map_surface, powerup_anim)

        # Blit map to virtual surface at the right position
        self.virtual_surface.blit(self.map_surface, (MAP_OFFSET_X, MAP_OFFSET_Y))

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

        # Get current player data from game state
        current_players = {p['player']: p for p in self.current_game_state['players']}

        for player_id in range(1, 5):
            y_pos = start_y + (player_id - 1) * player_height
            player_data = current_players.get(player_id)
            self.draw_single_player_stats(self.player_panel_surface, player_id, y_pos, player_height, player_data)

        # Blit to virtual surface
        self.virtual_surface.blit(self.player_panel_surface, (0, MAP_OFFSET_Y))

    def draw_single_player_stats(self, surface, player_id, y_pos, height, player_data):
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

        # Life - Draw hearts (use live data if available)
        current_health = player_data['health'] if player_data else stats['life']
        life_text = "Life:"
        life_surface = self.small_font.render(life_text, True, (255, 100, 120))
        surface.blit(life_surface, (80, stats_start_y))

        # Draw hearts for life count
        heart_start_x = 115
        for i in range(current_health):
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

        # Special abilities with actual powerup icons
        abilities_text = "Abilities:"
        abilities_surface = self.small_font.render(abilities_text, True, COLORS['TEXT_WHITE'])
        surface.blit(abilities_surface, (80, stats_start_y + stat_height * 4))

        # Filter to only show special abilities
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

        # Apply scale to colors for dead players
        if scale < 1.0:
            outfit_color = tuple(int(c * scale) for c in outfit_color)

        size = int(16 * scale)

        # Body
        body_rect = pygame.Rect(x - size // 2, y, size, int(size * 1.2))
        pygame.draw.rect(surface, outfit_color, body_rect)
        pygame.draw.rect(surface, tuple(max(0, c - 40) for c in outfit_color), body_rect, 1)

        # Head
        head_y = y - size // 2
        skin_color = tuple(int(c * scale) for c in COLORS['SKIN'])
        pygame.draw.circle(surface, skin_color, (x, head_y), size // 2)
        pygame.draw.circle(surface, tuple(max(0, c - 30) for c in skin_color), (x, head_y), size // 2, 1)

        # Simple face
        eye_color = (0, 0, 0) if scale > 0.8 else (100, 100, 100)
        pygame.draw.circle(surface, eye_color, (x - size // 4, head_y - 2), 1)
        pygame.draw.circle(surface, eye_color, (x + size // 4, head_y - 2), 1)

        # Player number badge
        badge_surf = pygame.Surface((12, 8), pygame.SRCALPHA)
        badge_alpha = int(200 * scale)
        pygame.draw.rect(badge_surf, (255, 255, 255, badge_alpha), (0, 0, 12, 8))
        pygame.draw.rect(badge_surf, (0, 0, 0), (0, 0, 12, 8), 1)

        num_text = self.small_font.render(str(player_num), True, (0, 0, 0))
        badge_surf.blit(num_text, (3, -2))
        surface.blit(badge_surf, (x - 6, y + int(size * 1.2) + 2))

    def draw_mini_heart(self, surface, x, y, color):
        """Draw a small heart icon"""
        size = 6
        pygame.draw.circle(surface, color, (x - 2, y - 1), 2)
        pygame.draw.circle(surface, color, (x + 2, y - 1), 2)
        points = [(x - 3, y), (x + 3, y), (x, y + 4)]
        pygame.draw.polygon(surface, color, points)

    def draw_mini_bomb(self, surface, x, y, color):
        """Draw a small bomb icon"""
        pygame.draw.circle(surface, color, (x, y + 2), 3)
        pygame.draw.line(surface, color, (x, y - 1), (x - 2, y - 3), 1)
        pygame.draw.circle(surface, (255, 200, 0), (x - 2, y - 3), 1)

    def draw_ability_powerup_icon(self, surface, x, y, ability):
        """Draw proper powerup icons for abilities"""
        size = 12

        ability_icon_map = {
            'kick_bomb': ('boot', (255, 100, 255)),
            'plus_bombs': ('factory', (255, 150, 100)),
            'phased': ('ghost', (200, 200, 255)),
            'freeze_bomb': ('freeze', (150, 200, 255)),
            'repeat_bombs': ('factory', COLORS['TEXT_GOLD']),
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
                elif event.key == pygame.K_r:
                    # Manual reload (not used in port mode, but kept for compatibility)
                    print("🔄 Manual reload (port mode active)")
                elif event.key == pygame.K_SPACE:
                    # Pause/unpause (could be extended)
                    print("⏸️ Pause/Resume (feature not implemented)")
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

                    # Check if click is within map area
                    if (MAP_OFFSET_X <= virtual_x < MAP_OFFSET_X + MAP_SIZE * TILE_SIZE and
                            MAP_OFFSET_Y <= virtual_y < MAP_OFFSET_Y + MAP_SIZE * TILE_SIZE):

                        # Adjust mouse coordinates relative to map
                        map_mouse_x = virtual_x - MAP_OFFSET_X
                        map_mouse_y = virtual_y - MAP_OFFSET_Y

                        tile_x = map_mouse_y // TILE_SIZE
                        tile_y = map_mouse_x // TILE_SIZE

                        if 0 <= tile_x < MAP_SIZE and 0 <= tile_y < MAP_SIZE:
                            self.selected_tile = (tile_x, tile_y)
                            tile_type = self.current_game_state['tiles'][tile_x][tile_y]
                            powerup = self.current_game_state['powerups'][tile_x][tile_y]

                            tile_names = {0: 'FREE', 1: 'WOOD_BARREL', 2: 'BRICK_WALL', 3: 'METAL_BARREL',
                                          4: 'PLAYER_START'}
                            tile_name = tile_names.get(tile_type, 'UNKNOWN')

                            # Check for players, bombs, explosions at this position
                            objects_here = []

                            for player in self.current_game_state['players']:
                                if player['x'] == tile_x and player['y'] == tile_y:
                                    objects_here.append(f"Player {player['player']} ({player['status']})")

                            for bomb in self.current_game_state['bombs']:
                                if bomb['x'] == tile_x and bomb['y'] == tile_y:
                                    objects_here.append(f"Bomb (timer: {bomb['timer']})")

                            for explosion in self.current_game_state['explosions']:
                                if explosion['x'] == tile_x and explosion['y'] == tile_y:
                                    objects_here.append(f"Explosion ({explosion['type']})")

                            objects_str = " + ".join(objects_here) if objects_here else "Empty"
                            print(f"🎯 Click: ({tile_x}, {tile_y}) = {tile_name} + {powerup} | {objects_str}")

        return True

    def run(self):
        """Main game loop with Erlang port communication"""
        print("🎮 Playing with Fire 2 - Live Game Viewer Started!")
        print("🔌 Reading from Erlang ports:")
        print("   1️⃣ Initial map from map_generator")
        print("   2️⃣ Real-time updates from cn_graphics_server")
        print("🖱️ Click tiles to inspect | ESC to exit")

        running = True
        while running:
            running = self.handle_events()

            # Read data from Erlang ports
            packets = self.read_port_data()
            if packets:
                self.handle_port_data(packets)

            # Only draw if we have a map
            if self.map_initialized and self.current_game_state:
                # Clear virtual surface with gradient background
                bg_rect = pygame.Rect(0, 0, WINDOW_WIDTH, WINDOW_HEIGHT)
                self.draw_gradient_rect(self.virtual_surface, COLORS['BACKGROUND'], COLORS['PANEL_BG'], bg_rect)

                # Draw complete live game visualization
                self.draw_enhanced_map()
                self.draw_player_stats_panel()
                self.draw_powerups_panel()

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

                # Display connection status
                if self.waiting_for_initial_map:
                    status_text = "⏳ Waiting for initial map..."
                    color = COLORS['TEXT_ORANGE']
                else:
                    status_text = "🔄 Live updates active"
                    color = COLORS['TEXT_CYAN']

                status_surface = self.small_font.render(status_text, True, color)
                self.screen.blit(status_surface, (10, 10))

            else:
                # Show waiting screen
                self.screen.fill(COLORS['BACKGROUND'])

                waiting_text = "⏳ Waiting for initial map from map_generator..."
                text_surface = self.font.render(waiting_text, True, COLORS['TEXT_WHITE'])
                text_rect = text_surface.get_rect(center=(self.current_width // 2, self.current_height // 2))
                self.screen.blit(text_surface, text_rect)

                instruction_text = "Make sure map_generator:start_grid_port() is called first"
                inst_surface = self.small_font.render(instruction_text, True, COLORS['TEXT_CYAN'])
                inst_rect = inst_surface.get_rect(center=(self.current_width // 2, self.current_height // 2 + 30))
                self.screen.blit(inst_surface, inst_rect)

            pygame.display.flip()
            self.clock.tick(FPS)

        pygame.quit()
        sys.exit()


if __name__ == "__main__":
    visualizer = CompleteGameVisualizer()
    visualizer.run()