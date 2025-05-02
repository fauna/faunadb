package fauna.model

import fauna.ast.EvalContext
import fauna.atoms._
import fauna.auth.Auth
import fauna.lang._
import fauna.lang.syntax._
import fauna.model.schema.{ NativeCollectionID, NativeIndex }
import fauna.repo._
import fauna.repo.query.Query
import fauna.storage._
import fauna.storage.index._
import fauna.storage.ir.DocIDV
import fauna.trace._

sealed class SchemaSet(scope: ScopeID, coll: CollectionID, underlying: EventSet)
    extends EventSet {

  def count = underlying.count

  def isFiltered: Boolean = true

  def isComposite: Boolean = false

  def filteredForRead(auth: Auth): Query[Option[EventSet]] = {
    auth.checkReadPermission(scope, coll.toDocID) map {
      if (_) Some(this) else None
    }
  }

  def sparseSnapshot(ec: EvalContext, keys: Vector[Event], ascending: Boolean) =
    underlying.sparseSnapshot(ec, keys, ascending)

  def snapshot(
    ec: EvalContext,
    from: Event,
    to: Event,
    size: Int,
    ascending: Boolean) =
    underlying.snapshot(ec, from, to, size, ascending)

  def history(
    ec: EvalContext,
    from: Event,
    to: Event,
    size: Int,
    ascending: Boolean) =
    underlying.history(ec, from, to, size, ascending)

  def sortedValues(
    ec: EvalContext,
    from: Event,
    to: Event,
    size: Int,
    ascending: Boolean) =
    underlying.sortedValues(ec, from, to, size, ascending)

  def shape =
    underlying.shape
}

object SchemaSet {
  import NativeIndex._

  // Schema indexes
  case class Databases(scope: ScopeID)
      extends SchemaSet(
        scope,
        DatabaseID.collID,
        Difference(List(
          IndexSet(
            DocumentsByCollection(scope),
            Vector(IndexTerm(DocIDV(DatabaseID.collID.toDocID)))),

          IndexSet(
            DatabaseByDisabled(scope),
            Vector(IndexTerm(true)))
        ))
      )

  case class Keys(scope: ScopeID)
      extends SchemaSet(
        scope,
        KeyID.collID,
        IndexSet(
          DocumentsByCollection(scope),
          Vector(IndexTerm(DocIDV(KeyID.collID.toDocID)))))

  case class Tokens(scope: ScopeID)
      extends SchemaSet(
        scope,
        TokenID.collID,
        IndexSet(
          DocumentsByCollection(scope),
          Vector(IndexTerm(DocIDV(TokenID.collID.toDocID)))))

  case class Credentials(scope: ScopeID)
      extends SchemaSet(
        scope,
        CredentialsID.collID,
        IndexSet(
          DocumentsByCollection(scope),
          Vector(IndexTerm(DocIDV(CredentialsID.collID.toDocID)))))

  case class Collections(scope: ScopeID)
      extends SchemaSet(
        scope,
        CollectionID.collID,
        IndexSet(DocumentsByCollection(scope),
                       Vector(IndexTerm(DocIDV(CollectionID.collID.toDocID)))))

  case class Indexes(scope: ScopeID)
      extends SchemaSet(
        scope,
        IndexID.collID,
        IndexSet(
          DocumentsByCollection(scope),
          Vector(IndexTerm(DocIDV(IndexID.collID.toDocID))))) {

    // FIXME: the below filtering is a hack to hide v10 collection
    // indexes until we can add a proper native index that excludes them.
    private def isHiddenIndex(e: EventSet.Elem[Event]) =
      Index
        .get(scope, e.value.docID.as[IndexID])
        .map(_.fold(true) { idx =>
          idx.isCollectionIndex || idx.isHidden
        })

    override def sparseSnapshot(
      ec: EvalContext,
      keys: Vector[Event],
      ascending: Boolean) =
      super
        .sparseSnapshot(ec, keys, ascending)
        .rejectMT(isHiddenIndex)

    override def snapshot(
      ec: EvalContext,
      from: Event,
      to: Event,
      size: Int,
      ascending: Boolean) =
      super
        .snapshot(ec, from, to, size, ascending)
        .rejectMT(isHiddenIndex)

    override def history(
      ec: EvalContext,
      from: Event,
      to: Event,
      size: Int,
      ascending: Boolean) =
      super
        .history(ec, from, to, size, ascending)
        .rejectMT(isHiddenIndex)

    override def sortedValues(
      ec: EvalContext,
      from: Event,
      to: Event,
      size: Int,
      ascending: Boolean) =
      super
        .sortedValues(ec, from, to, size, ascending)
        .rejectMT(isHiddenIndex)

  }

  case class Tasks(scope: ScopeID)
      extends SchemaSet(
        scope,
        TaskID.collID,
        IndexSet(DocumentsByCollection(scope),
                       Vector(IndexTerm(DocIDV(TaskID.collID.toDocID)))))

  case class UserFunctions(scope: ScopeID)
      extends SchemaSet(
        scope,
        UserFunctionID.collID,
        IndexSet(
          DocumentsByCollection(scope),
          Vector(IndexTerm(DocIDV(UserFunctionID.collID.toDocID)))))

  case class Roles(scope: ScopeID)
      extends SchemaSet(
        scope,
        RoleID.collID,
        IndexSet(DocumentsByCollection(scope),
                       Vector(IndexTerm(DocIDV(RoleID.collID.toDocID)))))

  case class AccessProviders(scope: ScopeID)
      extends SchemaSet(
        scope,
        AccessProviderID.collID,
        IndexSet(DocumentsByCollection(scope),
          Vector(IndexTerm(DocIDV(AccessProviderID.collID.toDocID)))))

  def unapply(set: EventSet): Option[(ScopeID, DocID)] = set match {
    case SchemaSet.Databases(scope)       => Some((scope, DatabaseID.collID.toDocID))
    case SchemaSet.Keys(scope)            => Some((scope, KeyID.collID.toDocID))
    case SchemaSet.Tokens(scope)          => Some((scope, TokenID.collID.toDocID))
    case SchemaSet.Credentials(scope)     => Some((scope, CredentialsID.collID.toDocID))
    case SchemaSet.Collections(scope)     => Some((scope, CollectionID.collID.toDocID))
    case SchemaSet.Indexes(scope)         => Some((scope, IndexID.collID.toDocID))
    case SchemaSet.Tasks(scope)           => Some((scope, TaskID.collID.toDocID))
    case SchemaSet.UserFunctions(scope)   => Some((scope, UserFunctionID.collID.toDocID))
    case SchemaSet.Roles(scope)           => Some((scope, RoleID.collID.toDocID))
    case SchemaSet.AccessProviders(scope) => Some((scope, AccessProviderID.collID.toDocID))
    case _                                => None
  }
}

object IndexSet {
  private val log = getLogger
}

case class IndexSet(
  config: IndexConfig,
  terms: Vector[IndexTerm],
  isFiltered: Boolean = true)
    extends EventSet {

  override def equals(other: Any): Boolean =
    other match {
      case o: IndexSet =>
        config.scopeID == o.config.scopeID &&
        config.id == o.config.id &&
        terms == o.terms &&
        isFiltered == o.isFiltered
      case _ => false
    }

  override def hashCode: Int =
    31 * config.scopeID.hashCode * config.id.hashCode * terms.hashCode * isFiltered.hashCode

  def count = config.partitions.toInt

  private def toIndexValue(evt: Event, minimize: Boolean): IndexValue = {
    val ts = evt.ts.resolve(if (minimize) Timestamp.Epoch else Timestamp.MaxMicros)
    IndexValue(evt.tuple, ts, evt.action.toSetAction)
  }

  def isComposite: Boolean = false

  def filteredForRead(auth: Auth): Query[Option[EventSet]] = {
    auth.checkUnrestrictedReadPermission(config.scopeID, config.id, terms) flatMap {
      if (_) {
        Query.some(this.copy(isFiltered = false))
      } else {
        auth.checkReadIndexPermission(config.scopeID, config.id, terms) map {
          if (_) Some(this) else None
        }
      }
    }
  }

  def sortedValues(ec: EvalContext, from: Event, to: Event, size: Int, ascending: Boolean) = {
    val (f, t) = if (config.values.nonEmpty) {
      val f = from.withDocID(DocID.MaxValue)
      val t = to.withDocID(DocID.MinValue)
      (f, t)
    } else {
      (from, to)
    }

    Store.sortedIndex(config, terms, toIndexValue(f, ascending), toIndexValue(t, ascending), size, ascending) collectMT {
      elemFromIndexValue(_)
    }
  }

  def history(ec: EvalContext, from: Event, to: Event, size: Int, ascending: Boolean) =
    Store.historicalIndex(config, terms, toIndexValue(from, ascending), toIndexValue(to, !ascending), size, ascending) map { page =>
      traceMsg(s"  INDEX: Read ${page.value.size} IDs for ${IndexKey(config.scopeID, config.id, terms)}")
      page
    } collectMT {
      elemFromIndexValue(_)
    }

  def sparseSnapshot(ec: EvalContext, keys: Vector[Event], ascending: Boolean) =
    Store.sparseCollection(
      config,
      terms map { _.value },
      ec.validTime,
      keys map { _.tuple },
      ascending) collectMT {
      snapshotElemFromIndexValue(ec, _)
    }

  def snapshot(
    ec: EvalContext,
    from: Event,
    to: Event,
    size: Int,
    ascending: Boolean) =
    Store.collection(
      config,
      terms map { _.value },
      ec.validTime,
      from.tuple,
      to.tuple,
      size,
      ascending) map { page =>
      traceMsg(s"  INDEX: Read ${page.value.size} IDs for ${IndexKey(config.scopeID, config.id, terms)}")
      page
    } collectMT {
      snapshotElemFromIndexValue(ec, _)
    } recoverWith {
      case e: RangeArgumentException =>
        // log and propagate error
        IndexSet.log.error(s"IndexSet scopeID:${config.scopeID} indexID:${config.id} terms:${config.terms}")
        Query.fail(e)
    }

  private[this] def elemFromIndexValue(v: IndexValue) =
    elemIfCollExists(v)(identity)

  private[this] def snapshotElemFromIndexValue(ec: EvalContext, v: IndexValue) =
    elemIfCollExists(v)(_.at(ec.validTime, Add))

  private[this] def elemIfCollExists(v: IndexValue)(f: SetEvent => SetEvent) = {
    val existsQ = v.docID.collID match {
      case NativeCollectionID(_) => Query.value(true)
      case _                     => Collection.exists(config.scopeID, v.docID.collID)
    }

    existsQ.map {
      Option.when(_)(selfElem(f(v.event)))
    }
  }

  def shape = EventSet.Shape(config)
}
