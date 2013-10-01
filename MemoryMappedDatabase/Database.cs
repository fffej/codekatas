using System;
using System.Collections.Generic;

namespace MemoryMappedDatabase
{
    public class Database : IDisposable
    {
        private Dictionary<string, Table> m_DefinedTables = new Dictionary<string, Table>();

        private bool isDisposed;

        public void AddTable(string tableName, TableDefinition tableDefinition)
        {
            CheckDisposed();

            if (m_DefinedTables.ContainsKey(tableName))
            {
                throw new DuplicateTableException();
            }
            m_DefinedTables.Add(tableName, new Table(tableDefinition));
        }

        public IEnumerable<Table> DefinedTables
        {
            get
            {
                CheckDisposed(); 
                return m_DefinedTables.Values;
            }
        }

        public Table this[string table]
        {
            get
            {
                CheckDisposed(); 
                return m_DefinedTables[table];
            }
        }

        public void Dispose()
        {
            isDisposed = true;
            m_DefinedTables = null;
        }

        private void CheckDisposed()
        {
            if (isDisposed) throw new ObjectDisposedException("Object already disposed");
        }
    }
}