#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PLASMOID_DIR="$HOME/.local/share/plasma/plasmoids"
WIDGET_ID="com.thalheim.alertmanager-widget"

echo "Installing Alertmanager Widget..."

# Create directory if it doesn't exist
mkdir -p "$PLASMOID_DIR"

# Remove existing widget if present
if [ -d "$PLASMOID_DIR/$WIDGET_ID" ]; then
  echo "Removing existing widget..."
  rm -rf "${PLASMOID_DIR:?}/${WIDGET_ID:?}"
fi

# Copy widget files
cp -r "$SCRIPT_DIR/plasmoid/$WIDGET_ID" "$PLASMOID_DIR/"

echo "Widget installed successfully!"

# Restart plasmashell to load the new widget
echo "Restarting Plasma Shell..."
systemctl restart --user plasma-plasmashell.service

echo ""
echo "To add the widget to your panel:"
echo "1. Right-click on your panel"
echo "2. Select 'Add Widgets...'"
echo "3. Search for 'Prometheus Alerts'"
echo "4. Drag it to your panel"
