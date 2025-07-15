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


class GNGameVisualizer:
    def __init__(self):
        # Window setup
        self.screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
        pygame.display.set_caption("🎮 GN Game Visualizer - Waiting for updates...")
        self.clock = pygame.time.Clock()

        # Fonts
        self.title_font = pygame.font.Font(None, 36)
        self.font = pygame.font.Font(None, 24)
        self.small_font = pygame.font.Font(None, 18)

        # Animation variables
        self.time = 0
        self.powerup_pulse = 0
        self.camera_shake = 0

        # Port communication
        self.port_buffer = b''
        self.map_received = False
        
        # Game state
        self.current_game_state = self.get_empty_game_state()
        self.player_stats = self.load_default_player_stats()

        # Animation systems
        self.player_animations = {}
        self.bomb_animations = {}
        self.explosion_animations = []
        self.powerup_animations = []
        self.game_effects = []
        self.status_effects = {}

        # Surfaces
        self.map_surface = pygame.Surface((MAP_SIZE * TILE_SIZE, MAP_SIZE * TILE_SIZE))
        self.player_panel_surface = pygame.Surface((PLAYER_PANEL_WIDTH, MAP_SIZE * TILE_SIZE))
        self.powerup_panel_surface = pygame.Surface((WINDOW_WIDTH, POWERUP_PANEL_HEIGHT))

        print("🎮 GN Game Visualizer initialized")
        print("📡 Waiting for map updates from GN graphics server...")

    def read_port_data(self):
        """Read data from GN graphics server via stdin"""
        try:
            ready, _, _ = select.select([sys.stdin], [], [], 0)
            if not ready:
                return None

            data = sys.stdin.buffer.read()
            if not data:
                return None

            self.port_buffer += data

            # Process complete packets (4-byte length prefix + data)
            packets = []
            while len(self.port_buffer) >= 4:
                packet_length = struct.unpack('>I', self.port_buffer[:4])[0]
                
                if len(self.port_buffer) >= 4 + packet_length:
                    packet_data = self.port_buffer[4:4 + packet_length]
                    self.port_buffer = self.port_buffer[4 + packet_length:]
                    packets.append(packet_data)
                else:
                    break

            return packets

        except Exception as e:
            return None

    def decode_erlang_data(self, binary_data):
        """Decode Erlang binary term"""
        try:
            # Simple decoder for development
            text = binary_data.decode('utf-8', errors='ignore')
            if text.startswith('[') and (text.endswith(']') or text.endswith('].')):
                if text.endswith('].'):
                    text = text[:-1]
                return eval(text)
            return None
        except Exception as e:
            print(f"❌ Error decoding: {e}")
            return None

    def handle_port_data(self, packets):
        """Handle received packets from GN graphics server"""
        for packet in packets:
            decoded_data = self.decode_erlang_data(packet)
            if decoded_data:
                print("🔄 Received map update from GN graphics server")
                success = self.process_map_update(decoded_data)
                if success and not self.map_received:
                    self.map_received = True
                    pygame.display.set_caption("🎮 GN Game Visualizer - Live Updates")

    def process_map_update(self, map_data):
        """Process map update from GN graphics server"""
        try:
            game_state = self.parse_game_state(map_data)
            if game_state:
                self.current_game_state = game_state
                return True
            return False
        except Exception as e:
            print(f"❌ Error processing update: {e}")
            return False

    def parse_game_state(self, erlang_grid):
        """Parse Erlang grid into game state"""
        game_state = {
            'tiles': [[0 for _ in range(MAP_SIZE)] for _ in range(MAP_SIZE)],
            'powerups': [['none' for _ in range(MAP_SIZE)] for _ in range(MAP_SIZE)],
            'bombs': [],
            'players': [],
            'explosions': []
        }

        if not erlang_grid or not isinstance(erlang_grid, list):
            return self.get_empty_game_state()

        tile_mapping = {
            'free': 0, 'breakable': 1, 'unbreakable': 2, 'strong': 3, 'player_start': 4
        }

        for row_idx in range(min(len(erlang_grid), MAP_SIZE)):
            for col_idx in range(min(len(erlang_grid[row_idx]), MAP_SIZE)):
                cell = erlang_grid[row_idx][col_idx]

                if len(cell) >= 4:
                    tile_type, powerup_type, bomb_info, player_info = cell[:4]
                    
                    # Transpose coordinates
                    x, y = col_idx, row_idx

                    # Parse tile
                    game_state['tiles'][x][y] = tile_mapping.get(tile_type, 0)
                    game_state['powerups'][x][y] = powerup_type if powerup_type != 'none' else 'none'

                    # Parse bombs
                    if bomb_info != 'none':
                        bomb = self.parse_bomb_info(bomb_info, x, y)
                        if bomb:
                            game_state['bombs'].append(bomb)

                    # Parse players
                    if player_info != 'none':
                        player = self.parse_player_info(player_info, x, y)
                        if player:
                            game_state['players'].append(player)

        return game_state

    def parse_bomb_info(self, bomb_info, x, y):
        """Parse bomb information"""
        try:
            if isinstance(bomb_info, str) and 'bomb' in bomb_info.lower():
                return {
                    'x': x, 'y': y, 'type': 'normal_bomb', 'timer': 3, 'owner': 'unknown',
                    'power': 2, 'animation_start': self.time
                }
            elif isinstance(bomb_info, tuple) and len(bomb_info) >= 2:
                bomb_type = bomb_info[0] if len(bomb_info) > 0 else 'normal_bomb'
                timer = int(bomb_info[1]) if len(bomb_info) > 1 and str(bomb_info[1]).isdigit() else 3
                power = int(bomb_info[3]) if len(bomb_info) > 3 and str(bomb_info[3]).isdigit() else 2
                return {
                    'x': x, 'y': y, 'type': str(bomb_type), 'timer': timer, 'owner': 'unknown',
                    'power': power, 'animation_start': self.time
                }
        except:
            pass
        return None

    def parse_player_info(self, player_info, x, y):
        """Parse player information"""
        try:
            if isinstance(player_info, str) and 'player_' in player_info:
                player_num = int(player_info.split('_')[1])
                return {
                    'player': player_num, 'x': x, 'y': y, 'health': 3, 'status': 'alive',
                    'direction': 'north', 'last_update': self.time
                }
            elif isinstance(player_info, tuple) and len(player_info) >= 2:
                player_id = player_info[0]
                if 'player_' in str(player_id):
                    player_num = int(str(player_id).split('_')[1])
                    health = int(player_info[1]) if len(player_info) > 1 and str(player_info[1]).isdigit() else 3
                    status = player_info[2] if len(player_info) > 2 else 'alive'
                    direction = player_info[3] if len(player_info) > 3 else 'north'
                    return {
                        'player': player_num, 'x': x, 'y': y, 'health': health, 'status': str(status),
                        'direction': str(direction), 'last_update': self.time
                    }
        except:
            pass
        return None

    def get_empty_game_state(self):
        """Empty game state"""
        return {
            'tiles': [[0 for _ in range(MAP_SIZE)] for _ in range(MAP_SIZE)],
            'powerups': [['none' for _ in range(MAP_SIZE)] for _ in range(MAP_SIZE)],
            'bombs': [],
            'players': [],
            'explosions': []
        }

    def load_default_player_stats(self):
        """Default player stats"""
        return {
            1: {'life': 3, 'speed': 1, 'bombs': 3, 'explosion_radius': 2, 'special_abilities': [], 'color': COLORS['PLAYER_1']},
            2: {'life': 2, 'speed': 2, 'bombs': 4, 'explosion_radius': 1, 'special_abilities': ['kick_bomb'], 'color': COLORS['PLAYER_2']},
            3: {'life': 4, 'speed': 1, 'bombs': 2, 'explosion_radius': 3, 'special_abilities': [], 'color': COLORS['PLAYER_3']},
            4: {'life': 1, 'speed': 3, 'bombs': 5, 'explosion_radius': 1, 'special_abilities': ['plus_bombs', 'phased', 'freeze_bomb'], 'color': COLORS['PLAYER_4']}
        }

    def update_all_animations(self):
        """Update all active animations"""
        current_time = self.time

        # Update player walking animations
        for player_id in list(self.player_animations.keys()):
            anim = self.player_animations[player_id]
            if anim['active']:
                elapsed = current_time - anim['start_time']
                if elapsed >= anim['duration']:
                    del self.player_animations[player_id]

        # Update bomb animations
        for pos in list(self.bomb_animations.keys()):
            anim = self.bomb_animations[pos]
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

    # Drawing utility methods (copied from map_live.py)
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

        # Draw player
        self.draw_enhanced_player_character(surface, x, y, player_num, outfit_color)

    def draw_map(self):
        """Draw the complete enhanced map"""
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

    def draw_player_panel(self):
        """Draw player stats panel"""
        self.player_panel_surface.fill(COLORS['UI_BACKGROUND'])
        
        # Title
        title_text = "PLAYERS"
        title_surface = self.title_font.render(title_text, True, COLORS['TEXT_GOLD'])
        title_shadow = self.title_font.render(title_text, True, COLORS['TEXT_SHADOW'])
        
        self.player_panel_surface.blit(title_shadow, (12, 12))
        self.player_panel_surface.blit(title_surface, (10, 10))
        
        # Player stats
        current_players = {p['player']: p for p in self.current_game_state['players']}
        
        for player_id in range(1, 5):
            y_pos = 60 + (player_id - 1) * 100
            player_data = current_players.get(player_id)
            stats = self.player_stats[player_id]
            
            # Player background with gradient
            bg_rect = pygame.Rect(10, y_pos, PLAYER_PANEL_WIDTH - 20, 90)
            bg_alpha = int(40 + 20 * math.sin(self.time * 2 + player_id))
            bg_surf = pygame.Surface((bg_rect.width, bg_rect.height), pygame.SRCALPHA)
            
            player_color = stats['color']
            for y in range(bg_rect.height):
                ratio = y / bg_rect.height
                alpha = int(bg_alpha * (1 - ratio * 0.5))
                color = (*player_color[:3], alpha)
                pygame.draw.line(bg_surf, color, (0, y), (bg_rect.width, y))
            
            self.player_panel_surface.blit(bg_surf, (bg_rect.x, bg_rect.y))
            
            # Player info
            status = "ACTIVE" if player_data else "WAITING"
            health = player_data['health'] if player_data else stats['life']
            
            player_text = f"PLAYER {player_id}"
            status_text = f"Status: {status}"
            health_text = f"Health: {health}"
            
            player_surface = self.font.render(player_text, True, COLORS['TEXT_WHITE'])
            status_surface = self.small_font.render(status_text, True, COLORS['TEXT_CYAN'])
            health_surface = self.small_font.render(health_text, True, (255, 100, 120))
            
            self.player_panel_surface.blit(player_surface, (20, y_pos + 10))
            self.player_panel_surface.blit(status_surface, (20, y_pos + 35))
            self.player_panel_surface.blit(health_surface, (20, y_pos + 55))

    def draw_info_panel(self):
        """Draw info panel"""
        self.powerup_panel_surface.fill(COLORS['UI_BACKGROUND'])
        
        # Title
        title_text = "GAME INFO"
        title_surface = self.title_font.render(title_text, True, COLORS['TEXT_GOLD'])
        title_shadow = self.title_font.render(title_text, True, COLORS['TEXT_SHADOW'])
        
        self.powerup_panel_surface.blit(title_shadow, (22, 17))
        self.powerup_panel_surface.blit(title_surface, (20, 15))
        
        # Stats
        bomb_count = len(self.current_game_state['bombs'])
        player_count = len(self.current_game_state['players'])
        
        stats_text = [
            f"Active Players: {player_count}",
            f"Active Bombs: {bomb_count}",
            f"Status: {'LIVE UPDATES' if self.map_received else 'WAITING FOR DATA'}"
        ]
        
        for i, text in enumerate(stats_text):
            color = COLORS['TEXT_CYAN'] if i == 2 and self.map_received else COLORS['TEXT_WHITE']
            text_surface = self.small_font.render(text, True, color)
            self.powerup_panel_surface.blit(text_surface, (20, 60 + i * 25))

    def handle_events(self):
        """Handle pygame events"""
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return False
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_ESCAPE:
                    return False
        return True

    def run(self):
        """Main game loop"""
        print("🎮 GN Game Visualizer Started!")
        print("📡 Listening for updates from GN graphics server...")
        
        running = True
        while running:
            running = self.handle_events()
            
            # Read port data
            packets = self.read_port_data()
            if packets:
                self.handle_port_data(packets)
            
            # Clear screen
            self.screen.fill(COLORS['BACKGROUND'])
            
            if self.map_received:
                # Draw game elements with enhanced graphics
                bg_rect = pygame.Rect(0, 0, WINDOW_WIDTH, WINDOW_HEIGHT)
                self.draw_gradient_rect(self.screen, COLORS['BACKGROUND'], COLORS['PANEL_BG'], bg_rect)
                
                self.draw_map()
                self.draw_player_panel()
                self.draw_info_panel()
                
                # Blit surfaces
                self.screen.blit(self.map_surface, (MAP_OFFSET_X, MAP_OFFSET_Y))
                self.screen.blit(self.player_panel_surface, (0, MAP_OFFSET_Y))
                self.screen.blit(self.powerup_panel_surface, (0, POWERUP_OFFSET_Y))
                
                # Status
                status_text = "🔄 Live Updates Active"
                status_surface = self.small_font.render(status_text, True, COLORS['TEXT_CYAN'])
                self.screen.blit(status_surface, (10, 10))
            else:
                # Waiting screen
                waiting_text = "⏳ Waiting for map updates from GN graphics server..."
                text_surface = self.font.render(waiting_text, True, COLORS['TEXT_WHITE'])
                text_rect = text_surface.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2))
                self.screen.blit(text_surface, text_rect)
                
                instruction_text = "Graphics server will send data automatically"
                inst_surface = self.small_font.render(instruction_text, True, COLORS['TEXT_CYAN'])
                inst_rect = inst_surface.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 + 30))
                self.screen.blit(inst_surface, inst_rect)
            
            pygame.display.flip()
            self.clock.tick(FPS)
        
        pygame.quit()
        sys.exit()


if __name__ == "__main__":
    visualizer = GNGameVisualizer()
    visualizer.run()