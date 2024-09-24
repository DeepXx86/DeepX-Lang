extends TextEdit

var language_keywords = [
	"if", "else", "while", "for", "func", "var", "import", 
	"write", "no_cap", "cap", "wait", "getLine", "deskRe", 
	"deskDel", "deskList", "deskCd", "deskMake", "get", "discord"
]

var current_suggestion = ""

onready var autocomplete_popup = $PopupMenu2

func _ready():
	autocomplete_popup.connect("id_pressed", self, "_on_autocomplete_item_selected")

	for Printword in ["print","write","wait"]:
		add_keyword_color(Printword, Color(0.429688, 0.598999, 1))
	for TrueCap in ["no_cap"]:
		add_keyword_color(TrueCap, Color(0.317618, 0.933594, 0.094818))
	for eCap in ["cap"]:
		add_keyword_color(eCap, Color(0.957031, 0.314493, 0.209351))
	for keyword in ["if","else","elif"]:
		add_keyword_color(keyword, Color(0.984375, 0.607544, 0.731191))
	for funcKey in ["def","function","func","import","get","discord"]:
		add_keyword_color(funcKey, Color(0.761459, 0.470588, 0.866667))
	for WhileKey in ["while", "for"]:
		add_keyword_color(WhileKey, Color(0.785156, 0.180954, 0.520818))
	for Geta in ["getLine", "int"]:
		add_keyword_color(Geta, Color(0.984375, 0.933246, 0.699829))
	for DeskT in ["deskDel","deskList","deskCd","deskMake","deskRe"]:
		add_keyword_color(DeskT, Color(0.18869, 0.894531, 0.861445))
	add_color_region('"','"',Color(0.925781, 0.754994, 0.589462))
	add_color_region("="," ", Color(0.528534, 0.789809, 0.980469))

func _input(event):

	if event is InputEventKey:

		if event.scancode == KEY_ENTER:
			if _should_indent_after_colon():
				_handle_auto_indentation()
				accept_event() 
			return 

		if event.scancode == KEY_TAB and autocomplete_popup.is_visible():
			_insert_suggestion(autocomplete_popup.get_item_text(autocomplete_popup.get_current_index()))
			autocomplete_popup.hide()
			return

		_update_autocomplete()

func _update_autocomplete():
	var line_text = get_line(cursor_get_line())
	var current_word = _get_current_word(line_text, cursor_get_column())

	autocomplete_popup.clear()

	for keyword in language_keywords:
		if keyword.begins_with(current_word):
			autocomplete_popup.add_item(keyword)

	if autocomplete_popup.get_item_count() > 0 and current_word.length() > 0:

		var cursor_pos = _get_cursor_screen_position_below()
		autocomplete_popup.set_position(cursor_pos)
		autocomplete_popup.show()
	else:
		autocomplete_popup.hide()

func _should_indent_after_colon() -> bool:
	var current_line_text = get_line(cursor_get_line())

	return current_line_text.strip_edges().ends_with(":")

func _handle_auto_indentation():
	var current_line_text = get_line(cursor_get_line())

	var indentation = ""
	for i in range(current_line_text.length()):
		if current_line_text[i] == " " or current_line_text[i] == "\t":
			indentation += current_line_text[i]
		else:
			break

	insert_text_at_cursor("\n" + indentation + "\t")

func _get_current_word(line: String, cursor_column: int) -> String:
	var word = ""
	for i in range(cursor_column - 1, -1, -1):
		if line[i] == " ":
			break
		word = line[i] + word
	return word

func _insert_suggestion(suggestion: String):
	var line_text = get_line(cursor_get_line())
	var current_word = _get_current_word(line_text, cursor_get_column())

	select(cursor_get_line(), cursor_get_column() - current_word.length(), cursor_get_line(), cursor_get_column())

	cut()

	insert_text_at_cursor(suggestion)

func _on_autocomplete_item_selected(id: int):
	_insert_suggestion(autocomplete_popup.get_item_text(id))
	autocomplete_popup.hide()

func _get_cursor_screen_position_below() -> Vector2:

	var line = cursor_get_line()
	var column = cursor_get_column()

	var char_size = Vector2(8, 16)
	var text_margin = Vector2(4, 4)

	var cursor_pos = Vector2(column * char_size.x, (line + 1) * char_size.y) + text_margin

	return get_global_position() + cursor_pos
