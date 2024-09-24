extends Control


func _ready():
	$MenuButton.get_popup().add_item("Open File")
	$MenuButton.get_popup().add_item("Save as")
	$MenuButton.get_popup().add_item("Quit")
	$MenuButton.get_popup().add_item("About")
	

	$MenuButton.get_popup().connect("id_pressed", self, "_on_item_pressed")


func _on_item_pressed(id):
	match id:
		0: 
			print("Open File selected") 
			$OpenFileDialog.popup()
		1: 
			print("Save as selected")
			$SaveFileDialog.popup()
		2: 
			print("Quit")
			get_tree().quit()
		3:
			print("About")
			$WindowDialog.popup()


func _on_OpenFileDialog_file_selected(path):
	print(path)
	var f = File.new()
	f.open(path, 1)
	$TextEdit.text = f.get_as_text()


func _on_SaveFileDialog_file_selected(path):
	var f = File.new()
	f.open(path, 2)
	f.store_string($TextEdit.text)
