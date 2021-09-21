
import json

from collections import namedtuple

from pathlib import Path
from datetime import date
from enum import unique, Enum, IntEnum
from typing import Any, Dict, Optional, Union

@unique
class Page(IntEnum):
	RATINGS = 0
	TAGGING = 1
	FILTER  = 2
	FILES   = 3

@unique
class Score(IntEnum):
	UNSPECIFIED     = 0
	EXTREMELY_LOW   = 1
	MODERATELY_LOW  = 2
	SLIGHTLY_LOW    = 3
	NEUTRAL         = 4
	SLIGHTLY_HIGH   = 5
	MODERATELY_HIGH = 6
	EXTREMELY_HIGH  = 7

@unique
class Status(IntEnum):
	UNSPECIFIED = 0
	PLANNING    = 1
	DROPPED     = 2
	IN_PROGRESS = 3
	COMPLETED   = 4
	VIEW_AGAIN  = 5
	REVIEWING   = 6

class Tag(object):

	def __init__(self,
		idnum:       int            = -1,
		instances:   int            = 1,
		name:        str            = "unnamed",
		description: str            = "",
		from_record: Dict[str, Any] = None,
	):
		if from_record is not None:
			self.idnum       = from_record["idnum"]
			self.instances   = from_record["instances"]
			self.name        = from_record["name"]
			self.description = from_record["description"]
		else:
			self.idnum       = idnum
			self.instances   = instances
			self.name        = name
			self.description = description

	def __eq__(self, other):
		if isinstance(other, str):
			return self.name == other
		return self.name == other.name

	def __ne__(self, other):
		if isinstance(other, str):
			return self.name != other
		return self.name != other.name

	def __hash__(self):
		return hash(self.name)

	def to_json_friendly(self):
		return {
			"idnum"       : self.idnum,
			"instances"   : self.instances,
			"name"        : self.name,
			"description" : self.description
		}

class Rating(object):

	def __init__(self,
		idnum=-1,
		title="Untitled",
		score=Score.UNSPECIFIED,
		status=Status.UNSPECIFIED,
		comments="",
		tags=set(),
		from_record=None,
	):
		if from_record is not None:
			self.idnum    = from_record["idnum"]
			self.title    = from_record["title"]
			self.score    = Score(from_record["score"])
			self.status   = Status(from_record["status"])
			self.comments = from_record["comments"]
			self.tags     = set(from_record["tags"])

			self.created  = date.fromisoformat(from_record.get("created", date.today().isoformat()))
			self.modified = date.fromisoformat(from_record.get("modified", date.today().isoformat()))
		else:
			self.idnum    = idnum
			self.title    = title
			self.score    = score
			self.status   = status
			self.comments = comments
			self.tags     = tags

			self.created  = date.today()
			self.modified = self.created

	def to_json_friendly(self):
		return {
			"idnum"    : self.idnum,
			"title"    : self.title,
			"score"    : self.score.value,
			"status"   : self.status.value,
			"comments" : self.comments,
			"tags"     : list(self.tags),
			"created"  : self.created.isoformat(),
			"modified" : self.modified.isoformat(),
		}

	def __repr__(self) -> str:
		return "%i, %s, %s, %s, %s, %s, %s, %s" % (
			self.idnum,
			self.title,
			self.score,
			self.status,
			self.comments,
			self.tags,
			self.created,
			self.modified
		)

class Search(Enum):
	COMMENTS  = 0
	ADVANCED  = 1
	TITLE     = 2
	NO_FILTER = 3 

class Sort(Enum):
	SCORE          = 0
	STATUS         = 1
	TITLE          = 2

class Model(object):

	def __init__(self, ratings: Optional[Dict[int, Rating]] = None, tags: Optional[Dict[str, Tag]] = None):

		self.ratings    = {} if ratings is None else ratings
		self.max_rating = -1

		self.tags       = {} if tags    is None else tags
		self.max_tags   = -1

	def clear(self) -> None:
		self.max_rating = -1
		self.ratings.clear()

		self.max_tag    = -1
		self.tags.clear()

	def change_tags(self, rating, tags):
		dirty = False

		for tag in tags:
			if tag not in self.tags:
				self.max_tags += 1
				self.tags[tag] = Tag(
					idnum=self.max_tags,
					name=tag,
					instances=0,
				)
				dirty = True
		
		for tag in rating.tags - tags:
			removed = self.tags[tag]
			
			removed.instances -= 1
			if removed.instances == 0:
				dirty = True

		for tag in tags - rating.tags: 
			self.tags[tag].instances += 1
		
		rating.tags = tags

		return dirty

	def attach_tag(self, rating: Rating, tag: Tag) -> bool:
		if tag.name in rating.tags:
			return False

		rating.tags.add(tag.name)
		tag.instances += 1
		return True

	def detach_tag(self, rating: Rating, tag: Tag) -> bool:
		if tag.name not in rating.tags:
			return False

		rating.tags.remove(tag.name)
		tag.instances -= 1
		return True

	def create_rating(self) -> Rating:
		self.max_rating += 1
		rating = Rating(idnum=self.max_rating)
		self.ratings[self.max_rating] = rating
		return rating

	def fill_in(self, kind, liststore, sift=lambda item: True, presort=False):
		
		liststore.clear()
		
		if   kind is Rating:
			
			results = 0
			for rating in self.ratings.values():
				if sift(rating):
					liststore.prepend([rating.title, rating.idnum])
					results += 1

			return results

		elif kind is Tag:

			results = 0
			for tag in self.tags.values():
				if sift(tag):
					liststore.prepend([tag.name])
					results += 1

			return results

		else:
			raise TypeError(
				"Target cannot be type %s" % type(kind)
			)

	def remove(self, target: Union[Rating, Tag]) -> None:
		if   isinstance(target, Rating):
			
			del self.ratings[target.idnum]

		elif isinstance(target, Tag):

			del self.tags[target.name]

			for rating in self.ratings.values():
				rating.tags.discard(target.name)

		else:
			raise TypeError(
				"Target cannot be type %s" % type(target)
			)

	def rename(self, tag: Tag, changed_to: str) -> None:

		if tag.name not in self.tags:
			raise KeyError("Model does not possess tag \"%s\"" % tag.name)
		
		if changed_to in self.tags:

			results = 0
			for rating in self.ratings.values():
				new_name = changed_to in rating.tags
				old_name = tag.name   in rating.tags

				if   new_name and old_name:
					rating.tags.remove(tag.name)
					results += 1
				elif new_name and not old_name:
					results += 1
				elif not new_name and old_name:
					rating.tags.remove(tag.name)
					rating.tags.add(changed_to)
					results += 1
				else:
					pass
			
			# if this new name exists on another tag, merge them
			self.tags[changed_to].instances = results

			# remove the old entry
			del self.tags[tag.name]

		else:

			# replace every use of the old name with the new
			for rating in self.ratings.values():
				if tag.name in rating.tags:
					rating.tags.remove(tag.name)
					rating.tags.add(changed_to)

			# remove the old entry
			del self.tags[tag.name]

			# change the name and move it to the new key 
			self.tags[changed_to] = tag
			tag.name = changed_to

	def vacuum(self) -> None:

		to_remove = []

		for key, value in self.tags.items():
			if value.instances == 0:
				to_remove.append(key)

		for tag in to_remove:
			del self.tags[tag]

	def iter_tags(self):
		return self.tags.values()

	def iter_ratings(self):
		return self.ratings.values()

	def from_json_friendly(self, data):
		self.clear()

		for item in data["tags"]:
			idnum = item["idnum"]

			if idnum > self.max_tags:
				self.max_tags = idnum

			self.tags[item["name"]] = Tag(from_record=item)

		for item in data["ratings"]:

			idnum = item["idnum"]

			if idnum > self.max_rating:
				self.max_rating = idnum

			self.ratings[idnum] = Rating(from_record=item)

	def to_json_friendly(self):
		return {
			"ratings" : [
				rating.to_json_friendly() for rating in self.ratings.values()
			],
			"tags"    : [
				tag.to_json_friendly()    for tag    in self.tags.values()
			],
		}

	def load(self, path):

		if path is None: return False

		if not isinstance(path, Path):
			path = Path(path)

		if not path.is_file():return False

		self.from_json_friendly(
			json.loads(
					path.read_text()
				)
		)

		return True
	
	def save(self, path):

		if path is None: return False

		if not isinstance(path, Path):
			path = Path(path)

		if path.is_file() or not path.exists():
			path.write_text(
				json.dumps(self.to_json_friendly(), indent=4)
			)
			return True
		
		return False
