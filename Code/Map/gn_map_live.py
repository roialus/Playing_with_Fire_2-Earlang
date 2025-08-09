import pygame
import sys
import math
import random
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


class EnhancedGNGameVisualizer:
    def __init__(self):
        # Make window resizable and start with smaller size
        initial_width = min(WINDOW_WIDTH, 900)
        initial_height = min(WINDOW_HEIGHT, 700)
        self.screen = pygame.display.set_mode((initial_width, initial_height), pygame.RESIZABLE)
        pygame.display.set_caption("🎮 Enhanced GN Game Visualizer")
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

        # Enhanced Animation systems (same as CN system)
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

        print("🎮 Enhanced GN Game Visualizer initialized")
        print("⏳ Waiting for enhanced map data from GN graphics server...")

    def read_port_data(self):
        """Read data from Enhanced GN graphics server via stdin"""
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
        """Decode Erlang binary term (enhanced)"""
        try:
            # Try to decode as binary term first (like CN system)
            try:
                import pickle
                # This won't work directly, but we can try to decode the Erlang term
                # For now, fall back to the string method
                pass
            except:
                pass
            
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
        """Handle received packets from Enhanced GN graphics server"""
        for packet in packets:
            try:
                # Try to decode as binary term first (like CN system)
                decoded_data = eval(packet.decode('utf-8'))
            except:
                decoded_data = self.decode_erlang_data(packet)
            
            if decoded_data:
                # Check if this is a movement confirmation
                if (isinstance(decoded_data, list) and len(decoded_data) == 2 and
                        decoded_data[0] == 'movement_confirmation'):

                    print("🏃 Received movement confirmation from GN")
                    self.handle_movement_confirmation(decoded_data[1])

                elif self.waiting_for_initial_map:
                    # First data should be initial map
                    print("🗺️ Received initial map from Enhanced GN graphics server")
                    success = self.process_initial_map(decoded_data)
                    if success:
                        self.waiting_for_initial_map = False
                        self.map_initialized = True
                        print("✅ Initial map loaded! Now listening for GN updates...")
                else:
                    # Subsequent data updates
                    print("🔄 Received update from Enhanced GN graphics server")
                    self.process_map_update(decoded_data)

    def handle_movement_confirmation(self, confirmation_data):
        """Handle immediate movement confirmation for players and bombs (same as CN)"""
        entity_type = confirmation_data['entity_type']
        entity_data = confirmation_data['entity_data']

        if entity_type == 'player':
            self.handle_player_movement_confirmation(entity_data)
        elif entity_type == 'bomb':
            self.handle_bomb_movement_confirmation(entity_data)

    def handle_player_movement_confirmation(self, player_data):
        """Handle immediate player movement confirmation (same as CN)"""
        player_id = player_data['player_id']
        from_pos = player_data['from_pos']
        to_pos = player_data['to_pos']
        direction = player_data['direction']
        speed = player_data['speed']

        # Calculate speed-aware animation duration
        base_duration = 0.4  # Base duration
        speed_multiplier = max(speed, 1)
        actual_duration = base_duration / speed_multiplier

        # Start immediate animation
        self.player_animations[player_id] = {
            'type': 'confirmed_walking',
            'start_pos': from_pos,
            'end_pos': to_pos,
            'direction': direction,
            'start_time': self.time,
            'duration': actual_duration,
            'speed': speed,
            'confirmed': True,
            'active': True
        }

        # Add speed effects if player is fast
        if speed > 1:
            self.create_speed_boost_effect(player_id, from_pos[0], from_pos[1], speed, direction)

        print(
            f"🏃 GN Player {player_id} movement confirmed: {from_pos} -> {to_pos} (speed: {speed}x, duration: {actual_duration:.2f}s)")

    def handle_bomb_movement_confirmation(self, bomb_data):
        """Handle immediate bomb movement confirmation (kicked bomb) (same as CN)"""
        bomb_id = tuple(bomb_data['bomb_id'])  # Convert to tuple for dict key
        from_pos = bomb_data['from_pos']
        to_pos = bomb_data['to_pos']
        direction = bomb_data['direction']
        bomb_type = bomb_data['type']
        owner = bomb_data['owner']

        # Bombs move at a fixed speed
        bomb_movement_duration = 0.3

        # Start immediate bomb movement animation
        self.bomb_animations[bomb_id] = {
            'type': 'moving',
            'start_pos': from_pos,
            'end_pos': to_pos,
            'direction': direction,
            'start_time': self.time,
            'duration': bomb_movement_duration,
            'bomb_type': bomb_type,
            'owner': owner,
            'confirmed': True,
            'active': True
        }

        # Add kick effect at the starting position
        self.create_bomb_kick_effect(from_pos[0], from_pos[1], direction, owner)

        print(f"💣 GN Bomb movement confirmed: {from_pos} -> {to_pos} (kicked by {owner})")

    def create_speed_boost_effect(self, player_id, x, y, speed, direction):
        """Create visual effect for speed boost (same as CN)"""
        self.game_effects.append({
            'type': 'speed_boost',
            'player_id': player_id,
            'x': x, 'y': y,
            'speed': speed,
            'direction': direction,
            'start_time': self.time,
            'duration': 0.8,
            'active': True
        })

    def create_bomb_kick_effect(self, x, y, direction, kicker):
        """Create visual effect when bomb is kicked (same as CN)"""
        self.game_effects.append({
            'type': 'bomb_kick',
            'x': x, 'y': y,
            'direction': direction,
            'kicker': kicker,
            'start_time': self.time,
            'duration': 0.5,
            'active': True
        })

    def process_initial_map(self, map_data):
        """Process initial map from Enhanced GN graphics server"""
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
        """Process real-time update from Enhanced GN graphics server"""
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
        """Handle window resizing (same as CN)"""
        self.current_width = max(new_width, MIN_WINDOW_WIDTH)
        self.current_height = max(new_height, MIN_WINDOW_HEIGHT)

        # Update scale factor to maintain aspect ratio
        self.scale_factor = min(
            self.current_width / WINDOW_WIDTH,
            self.current_height / WINDOW_HEIGHT
        )

        # Recreate screen surface
        self.screen = pygame.display.set_mode((self.current_width, self.current_height), pygame.RESIZABLE)

    # Import all the parsing, animation, and drawing methods from CN system
    # (Same methods as in map_live_port.py)
    
    def parse_complete_game_state(self, erlang_grid):
        """Parse complete game state including bombs, players, explosions (same as CN)"""
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

                    # Parse player information - include speed
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
            f"✅ GN Game state loaded - Players: {len(game_state['players'])}, Bombs: {len(game_state['bombs'])}, Explosions: {len(game_state['explosions'])}")
        return game_state

    def parse_bomb_info(self, bomb_info, x, y):
        """Parse bomb information from Erlang data (same as CN)"""
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
        """Parse current player position and status with speed (same as CN)"""
        try:
            # Handle simple player atom like 'player_1'
            if isinstance(player_info, str) and 'player_' in player_info:
                player_num = int(player_info.split('_')[1])
                return {
                    'player': player_num,
                    'x': x, 'y': y,
                    'health': 3,  # Default health
                    'speed': 1,  # Default speed
                    'status': 'alive',
                    'direction': 'north',
                    'last_update': self.time
                }
            elif isinstance(player_info, tuple) and len(player_info) >= 2:
                # Complex player info: (player_id, health, speed)
                player_id = player_info[0]
                health = int(player_info[1]) if len(player_info) > 1 and str(player_info[1]).isdigit() else 3
                speed = int(player_info[2]) if len(player_info) > 2 and str(player_info[2]).isdigit() else 1
                status = player_info[3] if len(player_info) > 3 else 'alive'
                direction = player_info[4] if len(player_info) > 4 else 'north'

                if 'player_' in str(player_id):
                    player_num = int(str(player_id).split('_')[1])
                    return {
                        'player': player_num,
                        'x': x, 'y': y,
                        'health': health,
                        'speed': speed,  # Includes actual speed
                        'status': str(status),
                        'direction': str(direction),
                        'last_update': self.time
                    }
        except (ValueError, TypeError, IndexError):
            pass
        return None

    def parse_explosion_info(self, explosion_info, x, y):
        """Parse explosion state information (same as CN)"""
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

    # Include all the animation detection and creation methods from CN system
    # (Same methods as in map_live_port.py)
    
    def detect_complete_game_changes(self, old_state, new_state):
        """Comprehensive change detection for all game elements (same as CN)"""

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
        """Detect player movements, direction changes, status changes (same as CN)"""
        old_player_dict = {p['player']: p for p in old_players}
        new_player_dict = {p['player']: p for p in new_players}

        for player_id, new_player in new_player_dict.items():
            if player_id in old_player_dict:
                old_player = old_player_dict[player_id]

                # Position change - only create animation if not already confirmed
                if ((old_player['x'], old_player['y']) != (new_player['x'], new_player['y']) and
                        player_id not in self.player_animations):
                    self.create_detailed_walking_animation(
                        player_id,
                        (old_player['x'], old_player['y']),
                        (new_player['x'], new_player['y']),
                        new_player.get('direction', 'north'),
                        new_player.get('speed', 1)  # Include speed
                    )
                    print(
                        f"🚶 GN Player {player_id} moved from ({old_player['x']}, {old_player['y']}) to ({new_player['x']}, {new_player['y']})")

                # Health change
                if old_player['health'] != new_player['health']:
                    if new_player['health'] < old_player['health']:
                        self.create_damage_effect(player_id, new_player['x'], new_player['y'])
                        print(f"💔 GN Player {player_id} took damage: {old_player['health']} -> {new_player['health']}")
                    else:
                        self.create_healing_effect(player_id, new_player['x'], new_player['y'])
                        print(f"💚 GN Player {player_id} healed: {old_player['health']} -> {new_player['health']}")

                # Status change (death, stun, etc.)
                if old_player['status'] != new_player['status']:
                    self.create_status_change_effect(player_id, new_player['x'], new_player['y'],
                                                     old_player['status'], new_player['status'])
                    print(f"⚡ GN Player {player_id} status change: {old_player['status']} -> {new_player['status']}")
            else:
                # New player appeared (respawn)
                print(f"✨ GN Player {player_id} spawned at ({new_player['x']}, {new_player['y']})")
                self.create_player_spawn_effect(player_id, new_player['x'], new_player['y'])

    # Include all the other detection, animation creation, and drawing methods from CN system
    # This would include all the methods from map_live_port.py such as:
    # - detect_bomb_lifecycle
    # - detect_explosion_changes  
    # - detect_tile_changes
    # - detect_powerup_changes
    # - create_detailed_walking_animation
    # - All the drawing methods (draw_enhanced_floor, draw_enhanced_brick_wall, etc.)
    # - update_all_animations
    # - load_player_stats
    # - get_fallback_game_state
    # And all the other methods...

    def load_player_stats(self):
        """Load player statistics (same as CN)"""
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
        """Fallback game state if no data received (same as CN)"""
        return {
            'tiles': [[0 for _ in range(MAP_SIZE)] for _ in range(MAP_SIZE)],
            'powerups': [['none' for _ in range(MAP_SIZE)] for _ in range(MAP_SIZE)],
            'bombs': [],
            'players': [],
            'explosions': [],
            'game_info': {'time': 0, 'round': 1, 'status': 'waiting'}
        }

    def update_all_animations(self):
        """Update all active animations (same as CN)"""
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
            if anim.get('type') == 'moving':
                # Moving bomb animation
                elapsed = current_time - anim['start_time']
                if elapsed >= anim['duration']:
                    del self.bomb_animations[pos]
            else:
                # Regular bomb countdown - keep active until explicitly removed
                elapsed = current_time - anim['start_time']
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

    # Complete enhanced drawing methods from CN system
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
        """Draw animated bomb with countdown and movement"""
        bomb_id = (bomb_data['x'], bomb_data['y'])
        actual_x, actual_y = x, y

        # Check if bomb is moving
        if bomb_id in self.bomb_animations:
            anim = self.bomb_animations[bomb_id]
            if anim.get('confirmed', False) and anim['type'] == 'moving':
                elapsed = self.time - anim['start_time']
                progress = min(elapsed / anim['duration'], 1.0)

                # Interpolate position
                start_x, start_y = anim['start_pos']
                end_x, end_y = anim['end_pos']

                current_x = start_x + (end_x - start_x) * progress
                current_y = start_y + (end_y - start_y) * progress

                # Convert to screen coordinates
                actual_x = current_y * TILE_SIZE
                actual_y = current_x * TILE_SIZE

        center_x = actual_x + TILE_SIZE // 2
        center_y = actual_y + TILE_SIZE // 2

        # Pulsing effect based on timer
        pulse_speed = 2.0 + (bomb_data['timer'] * 2)
        pulse = 0.8 + 0.2 * math.sin(self.time * pulse_speed)
        bomb_size = int(16 * pulse)

        # Main bomb body
        pygame.draw.circle(surface, COLORS['BOMB_BLACK'], (center_x, center_y), bomb_size)
        pygame.draw.circle(surface, (60, 60, 60), (center_x, center_y), bomb_size, 2)

        # Timer display
        timer_text = str(bomb_data['timer'])
        timer_surface = self.font.render(timer_text, True, (255, 255, 255))
        timer_rect = timer_surface.get_rect(center=(center_x, center_y + bomb_size + 15))
        surface.blit(timer_surface, timer_rect)

    def draw_enhanced_player_with_effects(self, surface, x, y, player_data):
        """Draw player with all status effects and speed-aware animations"""
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2

        player_num = player_data['player']
        speed = player_data.get('speed', 1)

        # Get player color with speed-based enhancement
        player_colors = {
            1: COLORS['PLAYER_1'], 2: COLORS['PLAYER_2'],
            3: COLORS['PLAYER_3'], 4: COLORS['PLAYER_4']
        }
        base_color = player_colors.get(player_num, COLORS['PLAYER_1'])

        # Enhance color based on speed
        if speed > 1:
            glow_intensity = min(speed * 0.2, 0.8)
            enhanced_color = tuple(min(255, int(c * (1 + glow_intensity))) for c in base_color)
        else:
            enhanced_color = base_color

        # Check for walking animation with speed awareness
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

        # Draw player character
        self.draw_enhanced_player_character(surface, char_x, char_y, player_num, enhanced_color)

        # Show speed boost indicator
        if speed > 1:
            self.draw_speed_indicator(surface, center_x, center_y + 30, speed)

    def draw_speed_indicator(self, surface, x, y, speed):
        """Draw speed boost indicator"""
        for i in range(min(speed - 1, 3)):
            arrow_x = x + (i - 1) * 8
            arrow_points = [
                (arrow_x - 3, y + 3),
                (arrow_x, y - 2),
                (arrow_x + 3, y + 3),
                (arrow_x, y + 1)
            ]
            pygame.draw.polygon(surface, (255, 255, 100), arrow_points)
            pygame.draw.polygon(surface, (255, 200, 0), arrow_points, 1)

    def draw_enhanced_player_character(self, surface, x, y, player_num, outfit_color):
        """Draw the actual player character"""
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2

        # Gentle bobbing animation
        bob_offset = math.sin(self.time * 4 + player_num * 1.5) * 2
        char_y = center_y + bob_offset

        # Body with gradient
        body_rect = pygame.Rect(center_x - 8, char_y - 2, 16, 20)
        self.draw_gradient_rect(surface, outfit_color,
                                tuple(max(0, c - 40) for c in outfit_color), body_rect)

        # Head
        head_y = char_y - 12
        pygame.draw.circle(surface, COLORS['SKIN'], (center_x, head_y), 10)

        # Simple eyes
        pygame.draw.circle(surface, (0, 0, 0), (center_x - 3, head_y - 2), 2)
        pygame.draw.circle(surface, (0, 0, 0), (center_x + 3, head_y - 2), 2)

        # Player number badge
        badge_text = str(player_num)
        badge_surface = self.small_font.render(badge_text, True, (255, 255, 255))
        badge_rect = badge_surface.get_rect(center=(center_x, char_y + 25))
        
        # Badge background
        bg_rect = badge_rect.inflate(6, 4)
        pygame.draw.rect(surface, (0, 0, 0, 150), bg_rect)
        surface.blit(badge_surface, badge_rect)

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

        # Draw all active game effects
        self.draw_all_game_effects(self.map_surface)

    def draw_all_game_effects(self, surface):
        """Draw all active game effects"""
        for effect in self.game_effects:
            if effect.get('type') == 'bomb_kick':
                self.draw_bomb_kick_effect(surface, effect)
            elif effect.get('type') == 'speed_boost':
                self.draw_speed_boost_effect(surface, effect)
            elif effect.get('type') == 'dust_cloud':
                self.draw_dust_cloud_effect(surface, effect)

    def draw_bomb_kick_effect(self, surface, effect):
        """Draw the kick effect when bomb starts moving"""
        elapsed = self.time - effect['start_time']
        progress = elapsed / effect['duration']

        if progress > 1.0:
            return

        center_x = effect['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = effect['x'] * TILE_SIZE + TILE_SIZE // 2

        # Draw impact burst
        burst_size = int(20 * progress)
        alpha = int(150 * (1 - progress))

        if burst_size > 0 and alpha > 0:
            for angle in range(0, 360, 45):
                end_x = center_x + int(burst_size * math.cos(math.radians(angle)))
                end_y = center_y + int(burst_size * math.sin(math.radians(angle)))
                pygame.draw.line(surface, (255, 200, 100), (center_x, center_y), (end_x, end_y), 3)

    def draw_speed_boost_effect(self, surface, effect):
        """Draw speed boost effect"""
        elapsed = self.time - effect['start_time']
        progress = elapsed / effect['duration']

        if progress > 1.0:
            return

        center_x = effect['y'] * TILE_SIZE + TILE_SIZE // 2
        center_y = effect['x'] * TILE_SIZE + TILE_SIZE // 2

        # Speed trail particles
        speed = effect['speed']
        for i in range(speed * 2):
            alpha = int(100 * (1 - progress) * (1 - i / (speed * 2)))
            if alpha > 0:
                particle_size = max(1, 4 - i // 2)
                speed_color = (100 + speed * 30, 150 + speed * 20, 255)
                pygame.draw.circle(surface, (*speed_color, alpha), (center_x, center_y), particle_size)

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
            pygame.draw.circle(dust_surf, (200, 180, 150, alpha), (cloud_size, cloud_size), cloud_size)
            surface.blit(dust_surf, (center_x - cloud_size, center_y - cloud_size))

    def draw_player_stats_panel(self):
        """Draw player statistics panel on the left side (same as CN)"""
        self.player_panel_surface.fill(COLORS['UI_BACKGROUND'])

        # Panel title
        title_text = "GN PLAYERS"
        title_surface = self.title_font.render(title_text, True, COLORS['TEXT_GOLD'])
        self.player_panel_surface.blit(title_surface, (10, 10))

        # Draw each player's stats 
        current_players = {p['player']: p for p in self.current_game_state['players']}
        
        for player_id in range(1, 5):
            y_pos = 60 + (player_id - 1) * 80
            player_data = current_players.get(player_id)
            
            if player_data:
                status_text = f"Player {player_id}: ACTIVE"
                health_text = f"Health: {player_data['health']}"
                speed_text = f"Speed: {player_data.get('speed', 1)}"
                
                if player_data.get('speed', 1) > 1:
                    speed_color = COLORS['TEXT_CYAN']
                else:
                    speed_color = COLORS['TEXT_WHITE']
            else:
                status_text = f"Player {player_id}: WAITING"
                health_text = "Health: -"
                speed_text = "Speed: -"
                speed_color = COLORS['TEXT_WHITE']
            
            status_surface = self.small_font.render(status_text, True, COLORS['TEXT_WHITE'])
            health_surface = self.small_font.render(health_text, True, COLORS['TEXT_WHITE'])
            speed_surface = self.small_font.render(speed_text, True, speed_color) 
            
            self.player_panel_surface.blit(status_surface, (20, y_pos))
            self.player_panel_surface.blit(health_surface, (20, y_pos + 20))
            self.player_panel_surface.blit(speed_surface, (20, y_pos + 40))

    def draw_info_panel(self):
        """Draw info panel (same as CN)"""
        self.powerup_panel_surface.fill(COLORS['UI_BACKGROUND'])

        # Title
        title_text = "GN GAME INFO"
        title_surface = self.title_font.render(title_text, True, COLORS['TEXT_GOLD'])
        self.powerup_panel_surface.blit(title_surface, (20, 15))

        # Stats
        bomb_count = len(self.current_game_state['bombs'])
        player_count = len(self.current_game_state['players'])

        stats_text = [
            f"Active Players: {player_count}",
            f"Active Bombs: {bomb_count}",
            f"Status: {'ENHANCED LIVE UPDATES' if self.map_initialized else 'WAITING FOR DATA'}"
        ]

        for i, text in enumerate(stats_text):
            color = COLORS['TEXT_CYAN'] if i == 2 and self.map_initialized else COLORS['TEXT_WHITE']
            text_surface = self.small_font.render(text, True, color)
            self.powerup_panel_surface.blit(text_surface, (20, 60 + i * 25))

    def handle_events(self):
        """Handle pygame events (same as CN)"""
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return False
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_ESCAPE:
                    return False
            elif event.type == pygame.VIDEORESIZE:
                self.handle_window_resize(event.w, event.h)
        return True

    def run(self):
        """Main game loop with Enhanced GN port communication"""
        print("🎮 Enhanced GN Game Visualizer Started!")
        print("🔌 Reading from Enhanced GN graphics server:")
        print("   ✨ Enhanced with movement confirmations")
        print("   🏃 Smooth speed-aware animations")
        print("   💣 Bomb movement tracking")
        print("   🎨 Advanced visual effects")
        print("🖱️ Click tiles to inspect | ESC to exit")

        running = True
        while running:
            running = self.handle_events()

            # Read data from Enhanced GN graphics server
            packets = self.read_port_data()
            if packets:
                self.handle_port_data(packets)

            # Only draw if we have a map
            if self.map_initialized and self.current_game_state:
                # Clear virtual surface with gradient background
                self.virtual_surface.fill(COLORS['BACKGROUND'])

                # Draw complete enhanced game visualization
                self.draw_enhanced_map()
                self.draw_player_stats_panel()
                self.draw_info_panel()

                # Blit surfaces to virtual surface
                self.virtual_surface.blit(self.map_surface, (MAP_OFFSET_X, MAP_OFFSET_Y))
                self.virtual_surface.blit(self.player_panel_surface, (0, MAP_OFFSET_Y))
                self.virtual_surface.blit(self.powerup_panel_surface, (0, POWERUP_OFFSET_Y))

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
                status_text = "🔄 Enhanced GN live updates active"
                color = COLORS['TEXT_CYAN']

                status_surface = self.small_font.render(status_text, True, color)
                self.screen.blit(status_surface, (10, 10))

            else:
                # Show waiting screen
                self.screen.fill(COLORS['BACKGROUND'])

                waiting_text = "⏳ Waiting for enhanced map data from GN graphics server..."
                text_surface = self.font.render(waiting_text, True, COLORS['TEXT_WHITE'])
                text_rect = text_surface.get_rect(center=(self.current_width // 2, self.current_height // 2))
                self.screen.blit(text_surface, text_rect)

                instruction_text = "Enhanced GN server will send data automatically with movement confirmations"
                inst_surface = self.small_font.render(instruction_text, True, COLORS['TEXT_CYAN'])
                inst_rect = inst_surface.get_rect(center=(self.current_width // 2, self.current_height // 2 + 30))
                self.screen.blit(inst_surface, inst_rect)

            pygame.display.flip()
            self.clock.tick(FPS)

        pygame.quit()
        sys.exit()


if __name__ == "__main__":
    visualizer = EnhancedGNGameVisualizer()
    visualizer.run()