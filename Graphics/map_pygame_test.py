import pygame
import sys
import math
import random

# Initialize Pygame
pygame.init()

# Enhanced Constants - NEW LAYOUT - OPTIMIZED SIZE
TILE_SIZE = 40  # Size of each tile in pixels
MAP_SIZE = 16   # Size of the map in tiles (16x16)
PLAYER_PANEL_WIDTH = 220  # Left panel for player stats
POWERUP_PANEL_HEIGHT = 140  # Bottom panel for power-ups - increased height
WINDOW_WIDTH = PLAYER_PANEL_WIDTH + MAP_SIZE * TILE_SIZE + 20  # Player panel + map + margin
WINDOW_HEIGHT = MAP_SIZE * TILE_SIZE + POWERUP_PANEL_HEIGHT + 20  # Map + power-ups + margin
# print("window size:", WINDOW_WIDTH, "x", WINDOW_HEIGHT)
MIN_WINDOW_WIDTH = 800  # Minimum width for the window
MIN_WINDOW_HEIGHT = 600 # Minimum height for the window
FPS = 60    # Frames per second for the game loop

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

    def load_player_stats(self):
        """Load player statistics"""
        """Default stats:
        life: 3
        speed: 1
        bombs: 1
        explosion_radius: 1
        special_abilities: []"""

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
        # New unified grid format from Erlang: {tile_type, powerup_type, bomb_type, player_id}
        # Convert Erlang atoms to Python strings and map to numbers for compatibility

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

        # Raw Erlang grid data (converted from the generated file)
        erlang_grid = [
            [('unbreakable', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none')],
            [('unbreakable', 'none', 'none', 'none'), ('player_start', 'none', 'none', 'player_1'),
             ('free', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('breakable', 'plus_bombs', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('breakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('player_start', 'none', 'none', 'player_2'), ('unbreakable', 'none', 'none', 'none')],
            [('unbreakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('strong', 'phased', 'none', 'none'),
             ('breakable', 'none', 'none', 'none'), ('breakable', 'remote_ignition', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('strong', 'kick_bomb', 'none', 'none'), ('breakable', 'kick_bomb', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none')],
            [('unbreakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('breakable', 'remote_ignition', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('breakable', 'move_speed', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('breakable', 'none', 'none', 'none'),
             ('breakable', 'remote_ignition', 'none', 'none'), ('unbreakable', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('breakable', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none')],
            [('unbreakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('breakable', 'plus_bombs', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('breakable', 'freeze_bomb', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('strong', 'plus_life', 'none', 'none'), ('strong', 'bigger_explosion', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('breakable', 'kick_bomb', 'none', 'none'), ('breakable', 'repeat_bombs', 'none', 'none'),
             ('breakable', 'none', 'none', 'none'), ('breakable', 'kick_bomb', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none')],
            [('unbreakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('strong', 'phased', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('breakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('breakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('breakable', 'plus_bombs', 'none', 'none'), ('breakable', 'kick_bomb', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('breakable', 'repeat_bombs', 'none', 'none'),
             ('breakable', 'repeat_bombs', 'none', 'none'), ('unbreakable', 'none', 'none', 'none')],
            [('unbreakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('breakable', 'repeat_bombs', 'none', 'none'), ('strong', 'freeze_bomb', 'none', 'none'),
             ('breakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('strong', 'bigger_explosion', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('breakable', 'none', 'none', 'none'),
             ('breakable', 'none', 'none', 'none'), ('strong', 'bigger_explosion', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none')],
            [('unbreakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('breakable', 'kick_bomb', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('breakable', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none'),
             ('breakable', 'none', 'none', 'none'), ('strong', 'kick_bomb', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('breakable', 'none', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('strong', 'plus_life', 'none', 'none'),
             ('breakable', 'kick_bomb', 'none', 'none'), ('unbreakable', 'none', 'none', 'none')],
            [('unbreakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('breakable', 'none', 'none', 'none'),
             ('breakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('breakable', 'plus_bombs', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('breakable', 'remote_ignition', 'none', 'none'), ('breakable', 'none', 'none', 'none'),
             ('breakable', 'none', 'none', 'none'), ('breakable', 'none', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('breakable', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none')],
            [('unbreakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('strong', 'plus_life', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('breakable', 'none', 'none', 'none'),
             ('breakable', 'remote_ignition', 'none', 'none'), ('breakable', 'bigger_explosion', 'none', 'none'),
             ('breakable', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none')],
            [('unbreakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none'),
             ('breakable', 'none', 'none', 'none'), ('strong', 'remote_ignition', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('breakable', 'plus_life', 'none', 'none'),
             ('breakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('breakable', 'none', 'none', 'none'), ('strong', 'bigger_explosion', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('breakable', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none')],
            [('unbreakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('breakable', 'none', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('strong', 'remote_ignition', 'none', 'none'),
             ('breakable', 'plus_bombs', 'none', 'none'), ('unbreakable', 'none', 'none', 'none'),
             ('breakable', 'plus_life', 'none', 'none'), ('unbreakable', 'none', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('breakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('breakable', 'plus_bombs', 'none', 'none'), ('breakable', 'kick_bomb', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none')],
            [('unbreakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('breakable', 'plus_bombs', 'none', 'none'), ('strong', 'move_speed', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('breakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('strong', 'phased', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('strong', 'plus_bombs', 'none', 'none'), ('strong', 'plus_life', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none')],
            [('unbreakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('breakable', 'plus_bombs', 'none', 'none'), ('unbreakable', 'none', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('strong', 'move_speed', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none')],
            [('unbreakable', 'none', 'none', 'none'), ('player_start', 'none', 'none', 'player_3'),
             ('free', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('strong', 'remote_ignition', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('free', 'none', 'none', 'none'), ('free', 'none', 'none', 'none'),
             ('player_start', 'none', 'none', 'player_4'), ('unbreakable', 'none', 'none', 'none')],
            [('unbreakable', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none'),
             ('unbreakable', 'none', 'none', 'none'), ('unbreakable', 'none', 'none', 'none')]
        ]

        # Convert to the format expected by the current visualizer
        tiles = []
        powerups = []
        player_starts = []

        for x in range(len(erlang_grid)):
            tile_row = []
            powerup_row = []

            for y in range(len(erlang_grid[x])):
                cell = erlang_grid[x][y]
                tile_type, powerup_type, bomb_type, player_id = cell

                # Convert tile type to number
                tile_num = tile_mapping.get(tile_type, 0)
                tile_row.append(tile_num)

                # Convert powerup to string
                powerup_str = powerup_mapping.get(powerup_type, 'none')
                powerup_row.append(powerup_str)

                # Track player starts
                if tile_type == 'player_start' and player_id != 'none':
                    player_num = int(player_id.split('_')[1])  # Extract number from 'player_1', etc.
                    player_starts.append({'player': player_num, 'x': x, 'y': y})

            tiles.append(tile_row)
            powerups.append(powerup_row)

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

    def draw_enhanced_map(self):
        """Draw the complete enhanced map on the right side"""
        self.map_surface.fill(COLORS['BACKGROUND'])

        # Update animations
        self.time += 1 / FPS
        self.powerup_pulse += 1 / FPS

        # Draw tiles
        for x in range(MAP_SIZE):
            for y in range(MAP_SIZE):
                pixel_x = y * TILE_SIZE
                pixel_y = x * TILE_SIZE

                tile_type = self.map_data['tiles'][x][y]
                powerup = self.map_data['powerups'][x][y]
                has_powerup = powerup != "none"

                #  Draw floor
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
                    # Find and draw player
                    for player in self.map_data['player_starts']:
                        if player['x'] == x and player['y'] == y:
                            self.draw_enhanced_player(self.map_surface, pixel_x, pixel_y, player['player'])
                            break

                # Selection highlight
                if self.selected_tile == (x, y):
                    highlight_surf = pygame.Surface((TILE_SIZE, TILE_SIZE), pygame.SRCALPHA)
                    pygame.draw.rect(highlight_surf, COLORS['SELECTION'], (0, 0, TILE_SIZE, TILE_SIZE))
                    self.map_surface.blit(highlight_surf, (pixel_x, pixel_y))

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
