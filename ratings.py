#!/usr/bin/python3

import re
import json
import random

from pathlib import Path
from typing import Any, Callable, Tuple

import gi
gi.require_version("Gtk", "3.0")
from gi.repository import Gtk


from tags import Filter, Predicate, Box, CompilationError, escape, tokenize, enum_subject_parser_factory, TERMINALS, compile, PredicateDefinitions
from model import Search, Sort, Score, Recommendation, Status, Page, Tag, Rating, Model

#
# filter setup
#

parser_score          = enum_subject_parser_factory(Score)
parser_recommendation = enum_subject_parser_factory(Recommendation)
parser_status         = enum_subject_parser_factory(Status)

def parser_random(string: str) -> float:

	if string == 'nan':
		raise ValueError("percentage cannot be nan")

	percent = float(string)

	if percent < 0.0 or 100.0 < percent:
		raise ValueError(f"percentage {percent} is not in range 0 to 100 inclusive")

	return percent

def action_random(percent: float, ignore: Any) -> bool:
	return random.random() * 100 <= percent

PREDICATES = PredicateDefinitions(
	action=lambda tag, rating: tag in rating.tags
)

count_pattern = re.compile(r"^(\d+)\s+of\s+(.*)$")

def parser_count(string: str) -> Tuple[Box[int], Callable[[Any], bool]]:

	items = count_pattern.fullmatch(string)

	if not items:
		raise ValueError(f"subject {string} is not of the form: <number> of <expression>")

	integer = int(items[1])

	if integer < 0:
		raise ValueError(f"counting must begin from 0 or greater, not {integer}")

	try:
		subroutine = Filter(items[2], PREDICATES)
	except CompilationError as e:
		# intercept the error and change the source
		# to the source that we're trying to compile
		e.reason = "in quotation: " + e.reason
		e.source = source
		raise e

	return (Box(integer), subroutine)

def action_count(subject: Tuple[Box[int], Callable[[Any], bool]], rating: Rating) -> bool:

	integer, subroutine = subject

	if integer.value != 0 and subroutine(rating):
		integer.value -= 1
		return True
	
	return False

def parser_eval(source):
	try:
		return Filter(source, PREDICATES)
	except CompilationError as e:
		# intercept the error and change the source
		# to the source that we're trying to compile
		e.reason = "in quotation: " + e.reason
		e.source = source
		raise e

(PREDICATES

	# filter ratings for a specific score
	.define("score",
		action=lambda score, rating: score == rating.score,
		parser=parser_score,
	)

	# filter for ratings with at least a certain score
	.define("min score",
		action=lambda score, rating: rating.score >= score,
		parser=parser_score,
	)

	# filter for ratings with at most a certain score
	.define("max score",
		action=lambda score, rating: rating.score <= score,
		parser=parser_score,
	)

	# filter for ratings with a specific recommendation level
	.define("recommendation",
		action=lambda rec, rating: rec == rating.recommendation,
		parser=parser_recommendation,
	)

	# filter for ratings with a specific status
	.define("status",
		action=lambda status, rating: status == rating.status,
		parser=parser_status,
	)

	# filter for ratings with at least a certain score
	.define("min score",
		action=lambda status, rating: rating.status >= status,
		parser=parser_status,
	)

	# filter for ratings with at most a certain score
	.define("max score",
		action=lambda status, rating: rating.status <= status,
		parser=parser_status,
	)

	# filter for ratings with a percent chance to include them
	.define("random",
		action=action_random,
		parser=parser_random,
		pure=False
	)

	# filter for a certain number of results at most
	.define("count",
		action=action_count,
		parser=parser_count,
		pure=False
	)

	# evaluate a string as an expression
	.define("eval",
		action=lambda function, rating: function(rating),
		parser=parser_eval,
		pure=False
	)
)

#
#


def create_rating_filter(filter_tab):

	search         = filter_tab.search

	if   search == Search.COMMENTS:
		criterion = filter_tab.query.lower()
	elif search == Search.TAGS:
		criterion = Filter(filter_tab.query, PREDICATES)
	elif search == Search.TITLE:
		criterion = filter_tab.query.lower()
	else:
		pass

	def function(rating):

		if   search == Search.COMMENTS:
			if rating.comments.lower().find(criterion) == -1:
				return False 
		elif search == Search.TAGS:
			if not criterion(rating):
				return False
		elif search == Search.TITLE:
			if rating.title.lower().find(criterion) == -1:
				return False 
		else:
			pass

		return True

	return function

def create_tagging_filter(filter_tab):

	search    = filter_tab.tags_search
	criterion = filter_tab.tags_query.lower()

	def function(tag):

		if   search == Search.COMMENTS:
			if tag.description.lower().find(criterion) == -1:
				return False 
		elif search == Search.TITLE:
			if tag.name.lower().find(criterion) == -1:
				return False 
		else:
			pass

		return True

	return (lambda item: True) if criterion == "" else function

class FilterTab(object):
	
	def __init__(self, builder, model):

		self._search          = builder.get_object("filter_search_combobox")
		self._tags_search     = builder.get_object("filter_tags_search_combobox")
		self._tags_ascending  = builder.get_object("filter_tags_ascending")
		self._tags_descending = builder.get_object("filter_tags_descending")
		self._sort            = builder.get_object("filter_sort_combobox")

		self._query           = builder.get_object("search_entry")
		self._tags_query      = builder.get_object("tagging_search_entry")

		self._ascending       = builder.get_object("filter_ascending")
		self._descending      = builder.get_object("filter_descending")

		self.model            = model

		self.reset()
		self.reset_tags()

	def reset(self):
		self.search         = Search.TITLE
		self.sort           = Sort.TITLE

	def reset_tags(self):
		self.tags_search    = Search.TITLE

    # BUG setting sorting does not actually work
    # only sorts by title regardless of function
	def configure_sorting(self, tree_sortable):

		# a descending order is a good default
		if self.descending:
			order = Gtk.SortType.DESCENDING
		else:
			order = Gtk.SortType.ASCENDING

		tree_sortable.set_sort_column_id(0, order)

		sort = self.sort

		if   sort == Sort.RECOMMENDATION:
			def sort_func(model, a, b, userdata):
				x = userdata[model.get(a, 1)].recommendation
				y = userdata[model.get(b, 1)].recommendation
				return 1 if x > y else -1 if x < y else 0
		elif sort == Sort.SCORE:
			def sort_func(model, a, b, userdata):
				x = userdata[model.get(a, 1)].score
				y = userdata[model.get(b, 1)].score
				return 1 if x > y else -1 if x < y else 0
		elif sort == Sort.STATUS:
			def sort_func(model, a, b, userdata):
				x = userdata[model.get(a, 1)].status
				y = userdata[model.get(b, 1)].status
				return 1 if x > y else -1 if x < y else 0
		elif sort == Sort.TITLE:
			def sort_func(model, a, b, userdata):
				x = userdata[model.get(a, 1)].title
				y = userdata[model.get(b, 1)].title
				return 1 if x > y else -1 if x < y else 0
		else:
			raise ValueError('Enum value "%s" unknown for Sort' % sort)

		tree_sortable.set_sort_func(1, sort_func, self.model.ratings)

	def configure_sorting_tags(self, tree_sortable):

		# a descending order is a good default
		if self.tags_descending:
			order = Gtk.SortType.DESCENDING
		else:
			order = Gtk.SortType.ASCENDING

		tree_sortable.set_sort_column_id(0, order)

	def _check_tag(self, criterion, tag):
		return True

	def create(self, kind):

		if   kind is Rating:
			return create_rating_filter(self)
		elif kind is Tag:
			return create_tagging_filter(self)
		else:
			# TODO
			raise TypeError("kind must be one of Rating or Tag")

	@property
	def query(self):
		return self._query.get_text()

	@property
	def tags_query(self):
		return self._tags_query.get_text()

	@property
	def ascending(self):
		return self._ascending.get_active()

	@ascending.setter
	def ascending(self, value):
		self._ascending.set_active(value)

	@property
	def descending(self):
		return self._descending.get_active()

	@descending.setter
	def descending(self, value):
		self._descending.set_active(value)

	@property
	def tags_ascending(self):
		return self._tags_ascending.get_active()

	@ascending.setter
	def tags_ascending(self, value):
		self._tags_ascending.set_active(value)

	@property
	def tags_descending(self):
		return self._tags_descending.get_active()

	@descending.setter
	def tags_descending(self, value):
		self._tags_descending.set_active(value)
	
	@property
	def search(self):
		return Search(int(self._search.get_active_id()))

	@search.setter
	def search(self, value):
		self._search.set_active_id(str(value.value))

	@property
	def tags_search(self):
		return Search(int(self._tags_search.get_active_id()))

	@tags_search.setter
	def tags_search(self, value):
		self._tags_search.set_active_id(str(value.value))
	
	@property
	def sort(self):
		return Sort(int(self._sort.get_active_id()))

	@sort.setter
	def sort(self, value):
		self._sort.set_active_id(str(value.value))

class EditorTab(object):

	def __init__(self, builder, model):

		self._idnum          = builder.get_object("id_label")
		self._title          = builder.get_object("title_entry")
		self._score          = builder.get_object("score_combobox")
		self._recommendation = builder.get_object("recommend_combobox")
		self._status         = builder.get_object("status_combobox")
		self._comments       = builder.get_object("comments_textview").get_buffer()
		self._tags           = builder.get_object("tags_textview").get_buffer()
		self._original_tags  = None

		self.model           = model

	def copy_to(self, rating):
		rating.title          = self.title
		rating.score          = self.score
		rating.recommendation = self.recommendation
		rating.status         = self.status
		rating.comments       = self.comments

		if self._original_tags != self._tags.get_text(*self._tags.get_bounds(), True):
			self.model.change_tags(rating, self.tags)

	def copy_from(self, value):
		self.idnum          = value.idnum
		self.title          = value.title
		self.score          = value.score
		self.recommendation = value.recommendation
		self.status         = value.status
		self.comments       = value.comments
		self.tags           = value.tags

	@property
	def idnum(self):
		return self._idnum.get_text()

	@idnum.setter
	def idnum(self, number):
		self._idnum.set_text(str(number))

	@property
	def title(self):
		return self._title.get_text()

	@title.setter
	def title(self, text):
		self._title.set_text(text)

	@property
	def score(self):
		return Score(int(self._score.get_active_id()))

	@score.setter
	def score(self, value):
		self._score.set_active_id(str(value.value))

	@property
	def recommendation(self):
		return Recommendation(int(self._recommendation.get_active_id()))

	@recommendation.setter
	def recommendation(self, value):
		self._recommendation.set_active_id(str(value.value))

	@property
	def status(self):
		return Status(int(self._status.get_active_id()))

	@status.setter
	def status(self, value):
		self._status.set_active_id(str(value.value))

	@property
	def comments(self):
		start, end = self._comments.get_bounds()
		return self._comments.get_text(start, end, True)

	@comments.setter
	def comments(self, text):
		self._comments.set_text(text)

	@property
	def tags(self):
		start, end = self._tags.get_bounds()
		tokens = tokenize(self._tags.get_text(start, end, True))
		return {token for position, token in tokens if token not in TERMINALS}

	@tags.setter
	def tags(self, tag_set):
		self._original_tags = ", ".join([escape(tag) for tag in tag_set])
		self._tags.set_text(self._original_tags)

	def clear(self):
		self.title          = ""
		self.score          = Score.UNSPECIFIED
		self.recommendation = Recommendation.UNSPECIFIED
		self.status         = Status.UNSPECIFIED
		self.comments       = ""
		self.tags           = ""

class FilesTab(object):

	def __init__(self, builder):

		self._path    = builder.get_object("filepath_entry")
		self._chooser = builder.get_object("file_chooser_button")

		self._chooser.set_current_folder(str(Path.home()))
		# self.update_path()

	@property
	def path(self):
		return self._path.get_text()

	@path.setter
	def path(self, text):
		self._path.set_text(text)

	def update_path(self):
		self.path = self._chooser.get_filename()

class TaggingTab(object):

	def __init__(self, builder, model):

		self._idnum          = builder.get_object("tagging_id_label")
		self._name           = builder.get_object("tagging_name_entry")
		self._description    = builder.get_object("tagging_description_textview").get_buffer()

		self._original_name  = None

		self.selected        = None
		self.selected_row    = None

		self.model           = model

		def sort_func(model, a, b, userdata):
			x = model.get(a, 0)
			y = model.get(b, 0)
			return 1 if x > y else -1 if x < y else 0

		self.liststore       = Gtk.ListStore(str)
		self.liststore.set_sort_column_id(0, Gtk.SortType.DESCENDING)
		self.liststore.set_sort_func(0, sort_func, self.model.tags)

		self.treeview        = builder.get_object("tagging_treeview")
		self.treeview.set_model(self.liststore)

		self.title_column    = Gtk.TreeViewColumn("Name", Gtk.CellRendererText(), text=0)
		self.treeview.append_column(self.title_column)

		self.results_label   = builder.get_object("tagging_results_label")
		self.instances_label = builder.get_object("tagging_instances_label")

		self.filter          = lambda item: True

	def copy_to(self, tag):

		name = self.name
		if self._original_name is not None and name != self._original_name:
			self.model.rename(tag, name)
			self.selected = None
			self.refresh()

		tag.description = self.description

	def copy_from(self, tag):
		self.idnum          = tag.idnum
		self.name           = tag.name
		self._original_name = self.name
		self.description    = tag.description

		if tag.instances == 1:
			self.instances_label.set_text("1 instance")
		else:
			self.instances_label.set_text("%i instances" % tag.instances)

	def change_selection(self):

		if self.selected is not None:
			self.copy_to(self.selected)

		model, tag = self.treeview.get_selection().get_selected()
		if tag is None:
			return

		name              = model.get_value(tag, 0)
		self.selected     = self.model.tags[name]
		self.selected_row = self.liststore[tag]

		self.copy_from(self.model.tags[name])

	def remove(self):

		self.model.remove(self.selected)

		tag = self.liststore.get_iter_first()
		while tag is not None:
			if self.liststore.get_value(tag, 0) == self.selected.name:
				has_next_row = self.liststore.remove(tag)
				break
			tag = self.liststore.iter_next(tag)

		if has_next_row or (tag := self.liststore.get_iter_first()) is not None:
			name              = self.liststore.get_value(tag, 0)
			self.selected     = self.model.tags[idnum]
			self.selected_row = self.liststore[path]
			self.treeview.set_cursor(path, self.title_column, False)
			self.editor.copy_from(self.selected)
		else:
			self.selected    = None
			self.name        = ""
			self.description = ""
			self.idnum       = -1
			self.instances_label.set_text("0 instances")

	def vacuum(self):

		self.model.vacuum()
		self.refresh()

	def refresh(self):

		results = self.model.fill_in(Tag, self.liststore, self.filter)
		self.results_label.set_text("%i results" % results)

		if self.selected is not None:
			for row in self.liststore:
				if row[0] == self.selected.name:
					self.selected_row = row
					break
		else:
			self.selected     = None
			self.selected_row = None

	@property
	def idnum(self):
		return self._idnum.get_text()

	@idnum.setter
	def idnum(self, number):
		self._idnum.set_text(str(number))

	@idnum.setter
	def idnum(self, number):
		self._idnum.set_text(str(number))

	@property
	def name(self):
		return self._name.get_text()

	@name.setter
	def name(self, value):
		self._name.set_text(value)
	
	@property
	def description(self):
		start, end = self._description.get_bounds()
		return self._description.get_text(start, end, True)

	@description.setter
	def description(self, text):
		self._description.set_text(text)

class Rater:

	#
	# Location of configuration file
	#

	CONFIG = Path.home() / ".ratings.json"

	def __init__(self):
		builder = Gtk.Builder.new_from_file("ratings.glade")

		self.selected     = None
		self.selected_row = None

		self.model     = Model() 

		self.editor    = EditorTab(builder, self.model)
		self.filter    = FilterTab(builder, self.model)
		self.files     = FilesTab(builder)
		self.tagging   = TaggingTab(builder, self.model)

		self.liststore = Gtk.ListStore(str, int)

		self.treeview  = builder.get_object("search_treeview")
		self.treeview.set_model(self.liststore)

		self.title_column = Gtk.TreeViewColumn("Title", Gtk.CellRendererText(), text=0)
		self.treeview.append_column(self.title_column)

		self.filter.configure_sorting(self.liststore)

		self.rater_window = builder.get_object("rater_window")
		self.rater_window.connect("destroy", self.exit_rater)
		self.rater_window.show_all()

		self.results_label = builder.get_object("results_label")

		self.message_window = builder.get_object("message_window")
		self.query_label    = builder.get_object("message_window_query_label")
		self.error_label    = builder.get_object("message_window_error_label")
		self.message_window.connect("destroy", self.on_close_message_button_clicked)

		builder.connect_signals(self)

		if Rater.CONFIG.is_file():
			config          = json.loads(Rater.CONFIG.read_text())
			file            = config["last_open"]
			self.files.path = file if file is not None else ""
			if not self.open_file():
				self.new_file()
		else:
			self.file = None
			self.new_file()

	def exit_rater(self, widget):
		Rater.CONFIG.write_text(
			json.dumps({"last_open": self.file}, indent=4)
		)
		Gtk.main_quit()

	def close_message_window(self):
		self.message_window.hide()

	def open_file(self):
		
		path = Path(self.files.path)

		if path is None or not path.is_file():
			return False

		self.model.from_json_friendly(json.loads(path.read_text()))

		results = self.model.fill_in(Tag, self.tagging.liststore)
		self.tagging.results_label.set_text("%i results" % results)

		results = self.model.fill_in(Rating, self.liststore)
		self.results_label.set_text("%s results" % results)

		tag               = self.liststore.get_iter_first()
		index             = self.liststore.get_value(tag, 1)
		self.selected     = self.model.ratings[index]
		path              = self.liststore.get_path(tag)
		self.selected_row = self.liststore[path]
		self.treeview.set_cursor(path, self.title_column, False)
		self.editor.copy_from(self.selected)

		self.file = self.files.path
		return True

	def save_file(self):

		if self.selected is not None:
			self.editor.copy_to(self.selected)

		path = Path(self.files.path)

		if path.is_file() or not path.exists():
			path.write_text(
				json.dumps(self.model.to_json_friendly(), indent=4)
			)

	def new_file(self):
		self.liststore.clear()
		self.model.clear()
		self.new_rating()

	def new_rating(self):

		if self.selected is not None:
			self.editor.copy_to(self.selected)
		
		self.selected = self.model.create_rating()

		tag = self.liststore.prepend(
			[self.selected.title, self.selected.idnum]
		)

		path = self.liststore.get_path(tag)
		self.selected_row = self.liststore[path]
		self.treeview.set_cursor(path, self.title_column, False)

		self.editor.copy_from(self.selected)

	def delete_rating(self):

		self.model.remove(self.selected)

		tag = self.liststore.get_iter_first()
		while tag is not None:
			if self.liststore.get_value(tag, 1) == self.selected.idnum:
				has_next_row = self.liststore.remove(tag)
				break
			tag = self.liststore.iter_next(tag)

		if has_next_row or (tag := self.liststore.get_iter_first()) is not None:
			idnum             = self.liststore.get_value(tag, 1)
			self.selected     = self.model.ratings[idnum]
			self.selected_row = self.liststore[path]
			self.treeview.set_cursor(path, self.title_column, False)
			self.editor.copy_from(self.selected)
		else:
			self.new_rating()

	def change_selection(self):

		if self.selected is not None:
			self.editor.copy_to(self.selected)

		model, tag = self.treeview.get_selection().get_selected()
		if tag is None:
			return

		idnum             = model.get_value(tag, 1)
		self.selected     = self.model.ratings[idnum]
		self.selected_row = self.liststore[tag]

		self.editor.copy_from(self.selected)

	def on_new_rating_button_clicked(self, widget):
		self.new_rating()

	def on_delete_rating_button_clicked(self, widget):
		self.delete_rating()

	def on_search_treeview_row_activated(self, widget, column, index):
		self.change_selection()

	def on_tagging_treeview_row_activated(self, widget, column, index):
		self.tagging.change_selection()

	def on_title_entry_changed(self, widget):
		if self.selected_row is not None:
			self.selected_row[0] = self.editor.title

	def on_filter_apply_clicked(self, widget, option_a=None, option_b=None):
		
		try:
			function = self.filter.create(Rating)
		except CompilationError as error:
			self.query_label.set_text(
				"%s%s" % (
					error.source if error.source else self.filter.query,
					"\n" + error.underline_string() if error.position else "",
				)
			)
			self.error_label.set_text(error.reason)
			self.message_window.show_all()
			return

		self.liststore.clear()
		results  = self.model.fill_in(Rating, self.liststore, function)
		self.results_label.set_text("%i results" % results)

		if self.selected is not None:
			for row in self.liststore:
				if row[1] == self.selected.idnum:
					self.selected_row = row
					break
		else:
			self.selected     = None
			self.selected_row = None

		self.filter.configure_sorting(self.liststore)

	def on_filter_tags_apply_clicked(self, widget, option_a=None, option_b=None):
		self.tagging.filter = self.filter.create(Tag)
		self.filter.configure_sorting_tags(self.tagging.liststore)
		self.tagging.refresh()

	def on_filter_reset_clicked(self, widget):
		self.filter.reset()

	def on_filter_tags_reset_clicked(self, widget):
		self.filter.reset_tags()

	def on_switch_page(self, widget, page, index):
		if self.selected is not None:
			self.editor.copy_to(self.selected)

		if index == Page.TAGGING.value:
			self.tagging.refresh()
		elif self.tagging.selected is not None:
			self.tagging.copy_to(self.tagging.selected)

	def on_file_chooser_button_file_set(self, other):
		self.files.update_path()

	def on_open_file_button_clicked(self, widget):
		self.open_file()

	def on_save_file_button_clicked(self, widget):
		self.save_file()

	def on_new_file_button_clicked(self, widget):
		self.new_file()

	def on_attach_tag_button_clicked(self, widget):
		if self.selected is not None and self.tagging.selected is not None:
			self.model.attach_tag(self.selected, self.tagging.selected)
			self.editor.tags = self.selected.tags

	def on_detach_tag_button_clicked(self, widget):
		if self.selected is not None and self.tagging.selected is not None:
			self.model.detach_tag(self.selected, self.tagging.selected)
			self.editor.tags = self.selected.tags

	def on_delete_tag_button_clicked(self, widget):
		if self.selected is not None and self.tagging.selected is not None:
			self.editor.tags = self.selected.tags
			self.tagging.remove()

	def on_vacuum_tag_button_clicked(self, widget):
		self.tagging.vacuum()

	def on_close_message_button_clicked(self, widget):
		self.close_message_window()

def main():
	rater = Rater()
	Gtk.main()

if __name__ == '__main__':
	main()