using System;
using System.Collections.Generic;
using System.IO.MemoryMappedFiles;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using NUnit.Framework;

namespace MemoryMappedDatabase
{
    [TestFixture]
    public class MemoryMappedFileTest
    {
        [Test]
        public void Foo()
        {
            var mm = MemoryMappedFile.CreateFromFile("foo");
            var stream = mm.CreateViewStream();
            var accessor = mm.CreateViewAccessor();

            
        }
    }


    [TestFixture]
    public class DatabaseTest
    {
        private Database database;
        private readonly TableDefinition emptyTableDefinition = new TableDefinition(null);

        private TableDefinition m_TableDefinition = new TableDefinition(
            new Dictionary<string, ColumnType>()
            {
                {"A", ColumnType.Int},
                {"B", ColumnType.Bool}
            });

        private Dictionary<string, IColumnValue> m_Row = new Dictionary<string, IColumnValue>
        {
            {"A", new IntValue(101)},
            {"B", new BoolValue(false)}
        };

        private Dictionary<string, IColumnValue> m_Row2 = new Dictionary<string, IColumnValue>
        {
            {"A", new IntValue(102)},
            {"B", new BoolValue(true)}
        };

        [SetUp]
        public void SetUp()
        {
            database = new Database();
        }

        [Test]
        public void Construct()
        {
            database.AddTable("Banana", emptyTableDefinition);
            Assert.That(database.DefinedTables.Count(), Is.EqualTo(1));
        }

        [Test]
        [ExpectedException(typeof (DuplicateTableException))]
        public void TableNamesAreUnique()
        {
            database.AddTable("Banana", emptyTableDefinition);
            database.AddTable("Banana", emptyTableDefinition);
        }

        [Test]
        public void CanQueryTableDefinition()
        {
            database.AddTable("Table", m_TableDefinition);

            var retrievedDefinition = database["Table"].Definition;
            Assert.That(m_TableDefinition, Is.EqualTo(retrievedDefinition));
        }

        [Test]
        public void CanAddRowToTable()
        {
            database.AddTable("Table", m_TableDefinition);
            database["Table"].AddRow(m_Row);
        }

        [Test]
        public void EachRowIsUnique()
        {
            database.AddTable("Table", m_TableDefinition);
            var k1 = database["Table"].AddRow(m_Row);
            var k2 = database["Table"].AddRow(m_Row2);

            Assert.That(k1, Is.Not.EqualTo(k2));

        }

        [Test]
        public void CanListTableRows()
        {
            database.AddTable("Table", m_TableDefinition);
            database["Table"].AddRow(m_Row);

            var rows = database["Table"].GetRows();

            Assert.That(rows.Count(), Is.EqualTo(1));
        }

        [Test]
        [ExpectedException(typeof (UnfilledRowException))]
        public void CannotAddUnfilledRows()
        {
            database.AddTable("Table", m_TableDefinition);
            database["Table"].AddRow( new Dictionary<string, IColumnValue>()
            {
                {"A", new BoolValue(false)}
            });
        }

        [Test]
        [ExpectedException(typeof(ObjectDisposedException))]
        public void CanCloseDatabase()
        {
            database.Dispose();

            database.AddTable("Table", emptyTableDefinition);
        }
    }

    [TestFixture]
    public class TableDefinitionTest
    {
        [Test]
        public void CanConstructATableDefinition()
        {
            var tableDefinition = new TableDefinition(
                new Dictionary<string, ColumnType>()
                {
                    {"A", ColumnType.Int},
                    {"B", ColumnType.Bool}
                });

            Assert.That(tableDefinition.ColumnCount, Is.EqualTo(2));
        }


    }
}
