import pygame
import sys
import os
import threading
import time

# Redirect stdout for Erlang communication, and suppress pygame startup output
sys.stdout = open(sys.stdout.fileno(), mode='w', buffering=1, encoding='utf-8', errors='replace')

# Initialize Pygame
pygame.init()
SCREEN_WIDTH, SCREEN_HEIGHT = 800, 600
screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))
pygame.display.set_caption("Playing With Fire 2")

# Colors
WHITE = (255, 255, 255)
BLACK = (0, 0, 0)
GRAY = (160, 160, 160)
RED = (200, 50, 50)
GREEN = (50, 200, 50)
BLUE = (50, 50, 200)
BG_COLOR = (78, 53, 36)
BUTTON_COLOR = (240, 120, 60)
BUTTON_HOVER = (255, 160, 100)
TEXT_COLOR = (30, 30, 30)

# Fonts
font = pygame.font.SysFont("arial", 48)
title_font = pygame.font.SysFont("impact", 80)
small_font = pygame.font.SysFont("arial", 32)

# Global state
current_screen = "main_menu"
dot_count = 1
dot_timer = pygame.time.get_ticks()
waiting_count = 0
choice_timer_start = 0

# Load images 
try:
    logo_img = pygame.image.load("assets/logo.png").convert_alpha()
    bomb_img = pygame.image.load("assets/bomb.png").convert_alpha()
except:
    # Create placeholder images if files don't exist
    logo_img = pygame.Surface((200, 100))
    logo_img.fill(BLUE)
    bomb_img = pygame.Surface((32, 32))
    bomb_img.fill(RED)

# Scale images to fit the screen
def scale_image_preserve_ratio(image, max_width=None, max_height=None):
    original_width, original_height = image.get_size()
    aspect_ratio = original_width / original_height

    if max_width and not max_height:
        width = max_width
        height = int(width / aspect_ratio)
    elif max_height and not max_width:
        height = max_height
        width = int(height * aspect_ratio)
    elif max_width and max_height:
        width = min(max_width, int(max_height * aspect_ratio))
        height = min(max_height, int(max_width / aspect_ratio))
    else:
        width, height = original_width, original_height

    return pygame.transform.smoothscale(image, (width, height))

# Draw text on the screen
def draw_text(text, y, center=True, size=48, color=BLACK):
    fnt = pygame.font.SysFont(None, size)
    surface = fnt.render(text, True, color)
    rect = surface.get_rect(center=(SCREEN_WIDTH // 2, y) if center else (20, y))
    screen.blit(surface, rect)

# Draw a screen template with a title
def draw_screen_template(title, title_y=100):
    screen.fill(BG_COLOR)
    title_surface = title_font.render(title, True, BLACK)
    title_rect = title_surface.get_rect(center=(SCREEN_WIDTH // 2, title_y))
    screen.blit(title_surface, title_rect)

# Draw a button with text
def draw_button(text, y, mouse_pos, width=300, height=60):
    rect = pygame.Rect(0, 0, width, height)
    rect.center = (SCREEN_WIDTH // 2, y)
    color = BUTTON_HOVER if rect.collidepoint(mouse_pos) else BUTTON_COLOR
    pygame.draw.rect(screen, color, rect, border_radius=10)
    draw_text(text, y, size=36, color=TEXT_COLOR)
    return rect

# Draw a play button with an icon
def draw_play_button_with_icon(y, mouse_pos):
    rect = pygame.Rect(0, 0, 300, 60)
    rect.center = (SCREEN_WIDTH // 2, y)
    color = BUTTON_HOVER if rect.collidepoint(mouse_pos) else BUTTON_COLOR
    pygame.draw.rect(screen, color, rect, border_radius=10)

    icon = scale_image_preserve_ratio(bomb_img, max_height=36)
    fnt = pygame.font.SysFont(None, 36)
    text_surface = fnt.render("Play", True, TEXT_COLOR)

    total_width = icon.get_width() + 10 + text_surface.get_width()
    icon_x = rect.centerx - total_width // 2 - 15
    text_x = rect.centerx - 25

    icon_rect = icon.get_rect()
    icon_rect.left = icon_x
    icon_rect.centery = y

    text_rect = text_surface.get_rect()
    text_rect.left = text_x
    text_rect.centery = y

    screen.blit(icon, icon_rect)
    screen.blit(text_surface, text_rect)

    return rect

# Show the main menu with logo and buttons
def show_main_menu(mouse_pos):
    screen.fill(BG_COLOR)

    logo_scaled = scale_image_preserve_ratio(logo_img, max_width=400)
    logo_rect = logo_scaled.get_rect(center=(SCREEN_WIDTH // 2, 120))
    screen.blit(logo_scaled, logo_rect)

    play_btn = draw_play_button_with_icon(300, mouse_pos)
    exit_btn = draw_button("Exit", 390, mouse_pos)
    return [("play_clicked", play_btn), ("exit_clicked", exit_btn)]

# Show loading screen with animated dots
def show_loading():
    global dot_count, dot_timer
    draw_screen_template("")

    if pygame.time.get_ticks() - dot_timer > 500:
        dot_count = (dot_count % 3) + 1
        dot_timer = pygame.time.get_ticks()

    draw_text("Connecting to server" + "." * dot_count, SCREEN_HEIGHT // 2, size=40)

# Show loading animation
def show_waiting(count):
    global dot_count, dot_timer
    draw_screen_template("Connecting to Game Servers")

    if pygame.time.get_ticks() - dot_timer > 500:
        dot_count = (dot_count % 3) + 1
        dot_timer = pygame.time.get_ticks()

    status_text = f"Waiting for connections: {count}/4" + "." * dot_count
    draw_text(status_text, SCREEN_HEIGHT // 2, size=36, color=WHITE)

    # Show connection status visually
    for i in range(4):
        x = SCREEN_WIDTH // 2 - 150 + i * 100
        y = SCREEN_HEIGHT // 2 + 80
        color = GREEN if i < count else GRAY
        pygame.draw.circle(screen, color, (x, y), 20)
        draw_text(f"GN{i + 1}", y + 40, center=False, size=24, color=WHITE)

# Show a bomb icon to indicate game mode
def show_player_choice(mouse_pos):
    global choice_timer_start
    if choice_timer_start == 0:
        choice_timer_start = time.time()

    draw_screen_template("Choose Your Mode")

    # Calculate remaining time
    elapsed = time.time() - choice_timer_start
    remaining = max(0, 60 - int(elapsed))

    # Show timer
    timer_color = RED if remaining <= 10 else WHITE
    draw_text(f"Time remaining: {remaining}s", 200, size=32, color=timer_color)
    draw_text("(Bot mode will be selected automatically if no choice is made)", 230, size=24, color=GRAY)

    # Auto-select bot if time runs out
    if remaining <= 0:
        send_event("choice_timeout")
        return []

    play_btn = draw_button("Play the Game", 350, mouse_pos, width=350)
    bot_btn = draw_button("Bot Mode", 430, mouse_pos, width=350)
    return [("play_game_clicked", play_btn), ("bot_clicked", bot_btn)]

# Show game setup screen with a loading animation
def show_game_setup():
    draw_screen_template("Game Setting Up")

    # Show a progress bar or loading animation
    progress_width = 400
    progress_height = 20
    progress_x = (SCREEN_WIDTH - progress_width) // 2
    progress_y = SCREEN_HEIGHT // 2

    pygame.draw.rect(screen, GRAY, (progress_x, progress_y, progress_width, progress_height))

    # Animated progress bar
    progress = (pygame.time.get_ticks() % 3000) / 3000.0
    filled_width = int(progress_width * progress)
    pygame.draw.rect(screen, GREEN, (progress_x, progress_y, filled_width, progress_height))

    draw_text("Initializing game world...", progress_y + 50, size=32, color=WHITE)

# Show error message with retry and return buttons
def show_error(mouse_pos):
    draw_screen_template("Failed To Connect")
    retry_btn = draw_button("Retry", 300, mouse_pos)
    return_btn = draw_button("Return To Main Menu", 380, mouse_pos)
    return [("retry_clicked", retry_btn), ("return_to_menu", return_btn)]

# Send an event to the Erlang process
def send_event(event_str):
    try:
        sys.stdout.write(event_str + "\n")
        sys.stdout.flush()
    except BrokenPipeError:
        pygame.quit()
        return

# Listen for commands from Erlang process
def listen_for_commands():
    def reader():
        global current_screen, waiting_count, choice_timer_start
        for line in sys.stdin:
            cmd = line.strip()
            if cmd.startswith("show_waiting:"):
                waiting_count = int(cmd.split(":")[1])
                current_screen = "show_waiting"
            elif cmd == "show_player_choice":
                choice_timer_start = 0  # Reset timer
                current_screen = cmd
            elif cmd in {"show_main_menu", "show_loading", "show_error", "show_game_setup", "start_game"}:
                if cmd == "show_main_menu":
                    choice_timer_start = 0  # Reset timer when returning to main menu
                current_screen = cmd

    threading.Thread(target=reader, daemon=True).start()


def main():
    global current_screen
    listen_for_commands()
    current_screen = "show_main_menu"

    clock = pygame.time.Clock()
    buttons = []

    running = True
    while running:
        mouse_pos = pygame.mouse.get_pos()
        screen.fill(BG_COLOR)

        if current_screen == "show_main_menu":
            buttons = show_main_menu(mouse_pos)
        elif current_screen == "show_loading":
            show_loading()
            buttons = []
        elif current_screen == "show_waiting":
            show_waiting(waiting_count)
            buttons = []
        elif current_screen == "show_player_choice":
            buttons = show_player_choice(mouse_pos)
        elif current_screen == "show_game_setup":
            show_game_setup()
            buttons = []
        elif current_screen == "show_error":
            buttons = show_error(mouse_pos)
        elif current_screen == "start_game":
            draw_screen_template("Game Starting!")
            draw_text("Entering the battlefield...", SCREEN_HEIGHT // 2, size=40, color=WHITE)
            buttons = []
        else:
            draw_text("Waiting for command...", SCREEN_HEIGHT // 2)
            buttons = []

        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                pygame.quit()
                running = False

            elif event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
                for label, rect in buttons:
                    if rect.collidepoint(event.pos):
                        send_event(label)
                        if label == "exit_clicked":
                            pygame.quit()
                            running = False

        pygame.display.flip()
        clock.tick(60)


if __name__ == "__main__":
    main()