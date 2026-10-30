# Changelog

## v4.0

### Roles & access
- **Administrator** — full access, including user management in System → Users.
- **Editor** — can update datasets, archive, and manage metadata (no user admin).
- **Viewer** — read-only: browse Overview, Report Mode, download CSVs; cannot edit data.

### User management (admins)
- Assign roles (Admin / Editor / Viewer) when creating users or from the Users table.
- **Reactivate** deactivated accounts.
- **Reset password** for any user (no current password required).
- **Delete permanently** with username confirmation.

### Activity log
- **Accounts** filter for user-management events.
- **Export CSV** download of the full log.

### Data safety
- Automatic backups before saves (local `data/backups/` and Neon writes).
- **Restore from backup** in Update data → collapsible backup list.

### UI
- Dark / light theme toggle in the navbar.
- Improved login screen and internal-dashboard messaging.
- Settings tab (account + password) for all users.
